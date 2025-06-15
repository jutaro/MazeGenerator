{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           GL
import           MazeGenerator
import           Types

import           Control.Concurrent
import           Data.Map                             as Map
import           Graphics.Rendering.OpenGL            (GLint, ($=))
import qualified Graphics.UI.GLUT                     as Glut
import           System.Environment                   (getArgs)
import qualified System.Metrics                       as EKG

import           Cardano.Logging.Prometheus.TCPServer (runPrometheusSimple)


mazeDims   :: (Int, Int)
screenDims :: (GLint, GLint)

mazeDims    = (56, 48)            -- empty cells in a maze
screenDims  = (800, 600)          -- initial window dimensions

emptyStatistics :: StatisticsTracer
emptyStatistics = Statistics 0 0 0.0

calcStats ::
     StatisticsTracer
  -> LoggingContext
  -> MazeTracer
  -> IO StatisticsTracer
calcStats Statistics {..} _ (MazeSolutionStep isNewRun) =
  pure $
    if isNewRun
      then Statistics (numRuns + 1) numRecursions
                      (fromIntegral (numRuns + 1) / fromIntegral numRecursions)
      else Statistics numRuns (numRecursions + 1)
                      (fromIntegral numRuns / fromIntegral (numRecursions + 1))
calcStats stats _ _ = pure stats

withStatistics :: Trace IO StatisticsTracer -> IO (Trace IO MazeTracer)
withStatistics tr =
  foldTraceM calcStats emptyStatistics $
    contramap unfold tr

main :: IO ()
main = do
    putStrLn "Maze Generator (c) by M.G.Meier 2014-15, 2025\n"

    initializeGL screenDims "MazeGenerator"

    -- setup tracer
    let trConfig = emptyTraceConfig
            { tcOptions = Map.fromList
                [([], [ConfSeverity (SeverityF (Just Info))
                        ,ConfBackend ([Stdout HumanFormatColoured, EKGBackend])])
                ]
            }
    trBase <- standardTracer
    ekgStore <- EKG.newStore
    trEkg  <- ekgTracer trConfig ekgStore
    configReflection <- emptyConfigReflection
    mazeTr <- mkCardanoTracer trBase mempty (Just trEkg) ["Maze"]

    statTr <- mkCardanoTracer trBase mempty (Just trEkg) ["MazeS"]
    configureTracers configReflection trConfig [statTr]

    mazeTr' <- withStatistics statTr
    configureTracers configReflection trConfig [mazeTr]

    runPrometheusSimple ekgStore (False, (Just "127.0.0.1"), 3003)

    -- finish setting up the tracer
    appState <- newMVar (emptyAppState (mazeTr <> mazeTr'))


    getArgs >>= \case
        ["--speedtest"] ->
            let howMany = 1000
            in do
                putStrLn ("generating " ++ show howMany ++ " mazes")
                Glut.displayCallback    $= glutDisplayCallback appState
                delayed 50              $ replicateM_ howMany (generateMaze appState) >> terminateMainLoop appState
                startMainLoop appState

        _ -> do
                showKeyBindings
                as <- readMVar appState
                print as
                Glut.displayCallback        $= glutDisplayCallback appState
                Glut.reshapeCallback        $= Just (glutReshapeCallback appState)
                Glut.keyboardMouseCallback  $= Just (glutInputCallback appState)
                delayed 50                  $ generateMaze appState
                watchAndDispatch appState
                startMainLoop appState

  where
    emptyAppState tr = initialAppState screenDims mazeDims tr

    -- this is used to schedule an action ahead of entering the GLUT mainloop
    -- !! there is no return from the mainloop !!
    delayed ms action = void $ forkIO $ do
      threadDelay $ ms * 1000
      action

    showKeyBindings = putStrLn
        "Key Bindings:\n\
        \  (Space)              - create new maze\n\
        \  (Enter)              - solve maze (animated)\n\
        \  (Arrow left/right)   - adjust maze width\n\
        \  (Arrow up/down)      - adjust maze height\n\
        \  +, -                 - change maze pattern via generation bias\n\
        \  F1                   - toggle step-by-step animated maze creation\n\
        \  Esc                  - quit"

  -- periodically polls the app state to check if a maze has to be generated or solved, triggering the appropriate action
    watchAndDispatch :: MVar AppState -> IO ()
    watchAndDispatch appState = void $ forkIO $
        forever $ do
            AppState{..} <- readMVar appState
            if
                | asAnimating -> pure ()                -- an anmation is running, skip all checks
                | asNeedBuild -> generateMaze appState
                | asNeedSolve -> solveMaze appState
                | otherwise   -> pure ()

            -- wait 50ms before polling again
            threadDelay $ 50 * 1000
