
# 📘 Tracing & Metrics Tutorial with the Maze Generator

This tutorial introduces the new tracing system for Cardano by walking through the [Maze Generator](https://github.com/jutaro/MazeGenerator) application. Each commit builds upon the last and is accompanied by a simple explanation here. Our goal is to help you understand how to integrate tracing and metrics into real-world Haskell applications.

## ✨ Introduction: Why Trace Dispatcher?

Modern systems demand observability — not only to debug errors, but to understand behavior, performance, and intent. For **Cardano**, this led to the creation of **Trace Dispatcher**, a structured tracing and metrics framework layered atop contravariant logging. It offers a unified interface to emit human-readable logs, machine-readable structured events, and even runtime metrics.

Yet despite its power, the Trace Dispatcher can appear daunting at first glance. That’s where this tutorial comes in.

### 🎯 Goals of This Tutorial

1. **For Cardano Developers**:
   This guide was conceived as an accessible entry point into Cardano's tracing infrastructure. If you're new to the ecosystem or curious about how trace-dispatcher integrates with a real application, this tutorial will light the way.

2. **For Tracing Enthusiasts & System Builders**:
   Even if you're not building on Cardano, Trace Dispatcher is a general-purpose tool. This tutorial introduces its core concepts — domain-specific tracers, human/machine formatting, severity filtering, metrics, and stateful trace folding — by walking through a clean, isolated use case: the Maze Generator.

### 🧩 Why the Maze Generator?

The [Maze Generator](https://github.com/jutaro/MazeGenerator) is a simple Haskell program that visually constructs and solves mazes. Its clear flow and recursive logic make it an ideal playground to demonstrate:

* Emitting domain-specific trace events
* Adding timestamps and durations
* Exporting Prometheus-compatible metrics
* Folding trace streams into live statistics

Each commit in this tutorial builds incrementally, showing you how to evolve an application from silence to full introspection — in five deliberate, understandable steps.

So whether you're building on Cardano or just seeking a tracing solution that aligns with functional programming elegance, **read on — and trace your way to clarity.**

## 🔧 Prerequisites

* Basic Haskell knowledge
* Stack or Cabal installed
* Familiarity with JSON logs or structured logging is helpful, but not required

---

### 💡 Commit 1: Initial Maze Generator Setup

**Summary**
This commit prepares the Maze Generator application for integrating tracing and metrics. It introduces the necessary project configuration and adds key dependencies required for observability.

---

**Changes made**
Added a cabal.project file referencing the local trace-dispatcher package from ../cardano-node/trace-dispatcher.
You have to adjust the path to match your local setup.

Or, if you are already using the Cardano ecosystem — for example via Nix — you can import trace-dispatcher from CHaP (the Cardano Haskell Package repository) instead.

* Extended the `build-depends` in the `.cabal` file with the following libraries:

  * `trace-dispatcher` – for structured tracing
  * `ekg-core` – for runtime metrics
  * `aeson` – for JSON encoding of trace events
  * `time` – for handling timestamps
  * `text` – for efficient Unicode text handling

  Enabled additional language extensions: OverloadedStrings



### 💡 Commit 2: A First Basic Tracer

**Summary**
This commit introduces the initial tracing infrastructure using Cardano’s `trace-dispatcher`. It defines and configures a tracer, wires it into the application state, and emits a first trace event when a new maze is generated.

---

**Changes made**

* **Added Tracer configuration in `Main.hs`**:

  * A `TraceConfig` is created with:

    * a severity filter (`ConfSeverity (Just Info)`)
    * console output backend (`ConfBackend [Stdout HumanFormatColoured]`)
  * The tracer pipeline is initialized:

    * `standardTracer` for writing trace messages to console
    * `ekgTracer` for supporting metrics
    * `mkCardanoTracer` and `configureTracers` for wiring up the configuration
  * Resulting tracer `mazeTr` is passed into application state.

    ```haskell
    appState <- newMVar (emptyAppState mazeTr)
    ```

* **Added data type `MazeTracer` modelling domain-specific trace messages in `Types.hs`**:

  ```haskell
  data MazeTracer = GenerateNewMaze
                  | SolveMaze
                  deriving (Show, Eq)
  ```

  This defines a domain-specific log message type for tracing events in the maze application. For now, it includes two constructors:

  * `GenerateNewMaze` – emitted when a new maze is built
  * `SolveMaze` – for future use when maze-solving is implemented


* **Added Typeclass instances for tracing integration in `Types.hs`**:

  The following instances are implemented to support structured logging:

  ```haskell
  instance LogFormatting MazeTracer where
      forMachine _ GenerateNewMaze = mconcat []
      forHuman GenerateNewMaze = ""
  ```

  This provides minimal formatting logic for machine-readable and human-readable logs.

  ```haskell
  instance MetaTrace MazeTracer where
      namespaceFor GenerateNewMaze = Namespace [] ["GenerateNew"]

      severityFor (Namespace _ ["GenerateNew"]) _ = Just Info
      severityFor _ _ = Nothing

      documentFor (Namespace _ ["GenerateNew"]) = Just "A new maze gets constructed"
      documentFor _ = Nothing

      allNamespaces = [Namespace [] ["GenerateNew"]]
  ```

  * Assigns a namespace `["GenerateNew"]` to `GenerateNewMaze`
  * Sets severity level to `Info`
  * Adds a documentation string for the trace event

  These instances allow tools like `trace-forward` to classify, filter, and display the events.

* **Extended Application state in `Types.hs`**:

  * `AppState` is updated to store the tracer.
  * `emptyAppState` takes a `Trace IO FormattedMessage` as argument.

  ```haskell
  emptyAppState tr = initialAppState screenDims mazeDims tr
  ```

* **Added logic to emit our first kind of trace message in `MazeGenerator.hs`**:

  * Inside `generateMaze`, a log message is emitted using:

    ```haskell
    traceWith asTracer GenerateNewMaze
    ```

    This marks the start of maze generation.


---

### 💡 Commit 3: A Useful Tracer

**Summary**
This commit extends the tracing logic to record meaningful, structured data about maze generation. It replaces the previous placeholder `MazeTracer` type with constructors that carry timestamps and durations. This enables precise measurement and logging of maze generation time.

---

**Changes made**

#### 🔄 In `MazeGenerator.hs`

* **Replaced `GenerateNewMaze` trace event with two time-sensitive events**:

  ```haskell
  traceWith asMazeTracer (GenerateNewMazeStart timestampStart)
  ...
  traceWith asMazeTracer (GenerateNewMazeEnd diff)
  ```
* **Captured timestamps** at the beginning and end of maze generation using `getCurrentTime`.
* **Calculated elapsed time** via `diffUTCTime`.

---

#### 🔄 In `Types.hs`

* **Modified `MazeTracer` data type**:

  ```haskell
  data MazeTracer
    = GenerateNewMazeStart UTCTime
    | GenerateNewMazeEnd NominalDiffTime
  ```

  This allows events to carry structured temporal data: the generation start time and the total duration.


* **Added `LogFormatting` instance**:

  * For machine-readable logs (`forMachine`), `UTCTime` and `NominalDiffTime` are serialized as strings.
  * For human-readable logs (`forHuman`), formatted messages include the timestamp or duration.

  Example:

  ```haskell
  forMachine _ (GenerateNewMazeEnd diffTime) = "duration" .= show diffTime
  ```

* **Added `MetaTrace` instance**:

  * Assigns namespaces: `["GenerateNewStart"]` and `["GenerateNewEnd"]`
  * Severity level: both events are logged with `Info`
  * Provides documentation strings for each event
  * Declares both namespaces in `allNamespaces`


### 💡 Commit 4: Simple Metrics

**Summary**
This commit adds support for a simple metric, and provides Prometheus metrics export via EKG.
The system now emits trace data and exposes runtime metrics over HTTP.
To observe the metrics visit: [localhost:3003/metrics](http://127.0.0.1:3003/metrics)

---

**Changes made**

#### 🔄 In `Main.hs`

* **Added a Prometheus server**:

  * The Prometheus-compatible metrics server is started using:

    ```haskell
    _ <- runPrometheusSimple ekgStore (False, Just "127.0.0.1", 3003)
    ```

    This launches an HTTP server on `127.0.0.1:3003`, exposing metrics collected in `ekgStore`.

* **Updated Backend in `TraceConfig`**:

  * `EKGBackend` is added to the list of trace backends:

    ```haskell
    ConfBackend [Stdout HumanFormatColoured, EKGBackend]
    ```

#### 🔄 In `MazeGenerator.hs`

  * **Added logic to emit a new kind of trace event in `solveMaze`**:

    ```haskell
    traceWith asMazeTracer (MazeSolutionStep True)
    ```

    This traces a step in the maze-solving process.

#### 🔄 In Types.hs

* **Added trace event `MazeSolutionStep Bool`**:

  ```haskell
  data MazeTracer
    = GenerateNewMazeStart UTCTime
    | GenerateNewMazeEnd NominalDiffTime
    | MazeSolutionStep Bool
  ```

  This constructor is used to represent individual steps during the maze solution process.
  The `Bool` payload allows a reset (True) and a step count (False).


* **Defined metric action**:

  ```haskell
  asMetrics (MazeSolutionStep False) = [CounterM "solution_steps" (Just 1)]
  asMetrics _                        = []
  ```

  * Defines an action on `MazeSolutionStep False`: Increments the counter metric named `solution_steps` by 1 on each said trace event.
  * Defines an action on `MazeSolutionStep True`: Noop on each said trace event.


* **Added a description of the newly added metric**:

  ```haskell
  metricsDocFor (Namespace _ ["MazeSolutionStep"]) =
    [ ("solution_steps", "Number of steps for a solution") ]
  ```


### 💡 Commit 5: Stateful Metrics with Derived Statistics

**Summary**
This commit introduces a stateful tracing layer that derives statistics from incoming trace events. It wraps the existing tracer using `foldTraceM`, enabling live computation of maze-solving metrics such as the number of runs, recursion steps, and their ratio.

---

**Changes made**

#### 🔄 New statistics infrastructure in `Main.hs`

* **Added a data type modelling domain-specific accumulated statistics**:

  ```haskell
  data Statistics = Statistics {
    numRuns :: Int,
    numRecursions :: Int,
    ratio :: Float
  }
  ```

* **Added function `calcStats` for accumulating statistics over time**:

   Implement a function that updates the accumulated domain-specific statistics based on the trace message.

  ```haskell
  calcStats :: StatisticsTracer -> LoggingContext -> MazeTracer -> IO StatisticsTracer
  ```

  * If the event is `MazeSolutionStep True`, it increments `numRuns`.
  * If `MazeSolutionStep False`, it increments `numRecursions`.
  * The `ratio` is recomputed accordingly.

* **Added function `withStatistics`**:
  Wraps a tracer with stateful accumulation logic:

  ```haskell
  withStatistics :: Trace IO StatisticsTracer -> IO (Trace IO MazeTracer)
  ```

  Internally uses `foldTraceM` to fold over emitted events and update the `StatisticsTracer`.

  ```haskell
  foldTraceM calcStats emptyStatistics $ contramap unfold tr
  ```
