module  Types
        ( MazeIx
        , GenerationBias(..)
        , AppState(..)
        , RenderCommand(..)
        , MazeTracer(..)
        , StatisticsTracer(..)

        , initialAppState
        , getQuadWH
        , emptyMaze

        , modifyAppState
        , modifyAppStateAndPrint

        , module ReExport
        ) where

import           Graphics.Rendering.OpenGL (GLfloat, GLint)

import           Control.Concurrent.MVar   as ReExport
import           Control.Monad             as ReExport
import           Data.Bool                 as ReExport

import           Cardano.Logging           as ReExport
import           Data.Aeson
import           Data.Text
import           Data.Time


-- a maze is defined by a list of maze indices [MazeIx].
-- these refer to all maze fields that are walkable.
type MazeIx = (Int, Int)


-- different visual patterns for the maze
data GenerationBias =
    NoBias | Vertical | Horizontal | CheckerBoard  | DiagonalSplit
    deriving (Show, Enum)

-- passed to the renderer callback
data RenderCommand =
      RenderFrame (GLfloat, GLfloat) (Maybe [MazeIx]) [MazeIx]
    | RendererQuit
    | RendererIdle

-- | The Tracer for the maze generation process.
data MazeTracer
  = GenerateNewMazeStart
      { genStartTime      :: UTCTime
      , genMazeWidthHight :: (Int, Int)
      }
  | GenerateNewMazeDuration
      { genDuration :: NominalDiffTime }
  | MazeSolutionStep Bool
  deriving (Show, Eq)

instance LogFormatting MazeTracer where
    forMachine _detailLevel (GenerateNewMazeStart time dim)  =
        mconcat [ "timestamp" .= (pack . show) time
                , "with" .= (pack . show) (fst dim)
                , "height" .= (pack . show) (snd dim)]
    forMachine _detailLevel (GenerateNewMazeDuration diffTime) =
        mconcat ["duration" .= (pack . show) diffTime]
    forMachine _detailLevel (MazeSolutionStep _) = mempty

    forHuman (GenerateNewMazeStart time dim) =
        "Start generating new maze time: " <> (pack . show) time
        <> " with: " <> (pack . show) (fst dim)
        <> " height: " <> (pack . show) (snd dim)
    forHuman (GenerateNewMazeDuration diffTime) =
        "End generating new maze duration: " <> (pack . show) diffTime
    forHuman (MazeSolutionStep _) = ""

    asMetrics (MazeSolutionStep False) = [CounterM "solution_steps" (Just 1)]
    asMetrics _                        = []

instance MetaTrace MazeTracer where
    namespaceFor GenerateNewMazeStart {} =
        Namespace [] ["GenerateNewStart"]
    namespaceFor GenerateNewMazeDuration {}   =
        Namespace [] ["GenerateNewDuration"]
    namespaceFor MazeSolutionStep {}   =
        Namespace [] ["MazeSolutionStep"]

    severityFor (Namespace _ ["GenerateNewStart"]) _ =
        Just Info
    severityFor (Namespace _ ["GenerateNewDuration"]) _   =
        Just Info
    severityFor (Namespace _ ["MazeSolutionStep"]) _   =
        Just Debug
    severityFor _ _                                  =
        Nothing

    documentFor (Namespace _ ["GenerateNewStart"]) =
        Just "A new maze gets constructed. Carries the start time"
    documentFor (Namespace _ ["GenerateNewDuration"]) =
        Just "A new maze was constructed. Carries the duration of constructing"
    documentFor _ = Nothing

    metricsDocFor (Namespace _ ["MazeSolutionStep"]) =
        [ ("solution_steps", "Number of steps for a solution")]
    metricsDocFor _ = []

    allNamespaces = [Namespace [] ["GenerateNewStart"],
                     Namespace [] ["GenerateNewDuration"],
                     Namespace [] ["MazeSolutionStep"]
                     ]


data StatisticsTracer = Statistics
  { numRuns           :: Int
  , numRecursions     :: Int
  , averageRecursions :: Double
  }

instance LogFormatting StatisticsTracer where
    forMachine _detailLevel (Statistics runs recursions avg) =
        mconcat [ "num_runs" .= runs
                , "num_recursions" .= recursions
                , "average_recursions" .= avg ]
    forHuman (Statistics runs recursions avg) =
        "Runs: " <> pack (show runs)
        <> ", Recursions: " <> pack (show recursions)
        <> ", Average Recursions: " <> pack (show avg)

    asMetrics (Statistics runs recursions avg) =
        [ IntM "num_runs" (fromIntegral runs)
        , IntM "num_recursions" (fromIntegral recursions)
        , DoubleM "average_recursions" avg]

instance MetaTrace StatisticsTracer where
    namespaceFor _ = Namespace [] ["Statistics"]

    severityFor (Namespace _ ["Statistics"]) _ = Just Debug
    severityFor _ _                            = Nothing

    documentFor (Namespace _ ["Statistics"]) = Just "Statistics about the maze solving"
    documentFor _ = Nothing

    metricsDocFor (Namespace _ ["Statistics"]) =
        [ ("num_runs", "Number of runs")
        , ("num_recursions", "Number of recursions")
        , ("average_recursions", "Average number of recursions per run") ]
    metricsDocFor _ = []

    allNamespaces = [Namespace [] ["Statistics"]]

-- application state
data AppState = AppState
    { asMaze        :: [MazeIx]                                         -- ^ the current maze
    , asDims        :: (Int, Int)                                       -- ^ maze dimensions
    , asQuadWH      :: (GLfloat, GLfloat)                               -- ^ quad dimensions on screen
    , asScreenWH    :: (GLint, GLint)                                   -- ^ current screen resolution
    , asNeedBuild   :: Bool                                             -- ^ need to build a new maze?
    , asNeedSolve   :: Bool                                             -- ^ need to find a solution?
    , asShowBuild   :: Bool                                             -- ^ animate the generation process?
    , asBuildBias   :: GenerationBias                                   -- ^ bias will influence the maze pattern
    , asAnimating   :: Bool                                             -- ^ some animation is in progress
    , asSolution    :: Maybe [MazeIx]                                   -- ^ if a solution has been found, remember it
    , asRenderFrame :: RenderCommand -> IO ()                           -- ^ send a render command to GLUT
    , asMazeTracer  :: Trace IO MazeTracer                              -- ^ tracer for the maze generation process
    }


instance Show AppState where
    show as = "AppState -- animate: " ++ show (asShowBuild as) ++ "; bias: " ++ show (asBuildBias as)

modifyAppState, modifyAppStateAndPrint :: MVar AppState -> (AppState -> AppState) -> IO ()
modifyAppState mv f =
    let f' a = pure (f a)
    in modifyMVar_ mv f'
modifyAppStateAndPrint mv f =
    let f' a = pure (f a, f a)
    in modifyMVar mv f' >>= print

-- project a maze coordinate (empty cell) onto its corresponding index in the Maze data structure
mazeProject :: Int -> Int
mazeProject = (+1) . (*2)

-- calculate quad size in pixels of a maze cell, depending on screen size and maze dimensions
getQuadWH :: (GLint, GLint) -> (Int, Int) -> (GLfloat, GLfloat)
getQuadWH (w, h) (mazeW, mazeH) =
    ( fromIntegral w / fromIntegral (mazeProject mazeW)
    , fromIntegral h / fromIntegral (mazeProject mazeH)
    )

emptyMaze :: (Int, Int) -> [MazeIx]
emptyMaze (mazeW, mazeH) =
    [ (mazeProject x, mazeProject y)
        | x <- [0 .. mazeW-1]
        , y <- [0 .. mazeH-1]
    ]

initialAppState :: (GLint, GLint) -> (Int, Int) -> Trace IO MazeTracer -> AppState
initialAppState screenDims mazeDims tracer =
    AppState
        { asMaze        = emptyMaze mazeDims
        , asDims        = mazeDims
        , asQuadWH      = getQuadWH screenDims mazeDims
        , asScreenWH    = screenDims
        , asNeedBuild   = False
        , asNeedSolve   = False
        , asShowBuild   = False
        , asBuildBias   = NoBias
        , asAnimating   = False
        , asSolution    = Nothing
        , asRenderFrame = \_ -> pure ()
        , asMazeTracer  = tracer
        }


