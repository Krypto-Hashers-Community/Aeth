# Architecture Overview

This document explains the internal design of Aeth.

## Core Concept

Aeth supports polymorphic command execution in two modes:

1. **Raw Mode:** Traditional string-based I/O (like Bash)
2. **Structured Mode:** Type-safe data structures (like Nushell)

The user chooses the mode with a prefix:

-   `ls` - Raw mode (returns text)
-   `@ls` - Structured mode (returns table data)

---

## Module Structure

```
app/
  Main.hs                    -- Entry point

src/Aeth/
  Types.hs                   -- Core types
  Parse.hs                   -- Command parser
  Exec.hs                    -- Command execution
  Structured.hs              -- Structured value types and commands
  ShellFast.hs               -- Main shell (haskeline-based)
  ConfigFast.hs              -- Simple key=value config parser
```

---

## Core Types

### OutputMode

```haskell
data OutputMode = RawString | Structured
```

### Segment

A single command in a pipeline:

```haskell
data Segment = Segment
  { segMode :: OutputMode
  , segName :: T.Text
  , segArgs :: [T.Text]
  }
```

### Pipeline

```haskell
newtype Pipeline = Pipeline { unPipeline :: [Segment] }
```

### ShellState

```haskell
data ShellState = ShellState
  { cwd            :: FilePath
  , envOverrides   :: Map String String
  , lastExitCode   :: Int
  , lastDurationMs :: Maybe Int
  , history        :: [String]
  , backgroundJobs :: [BackgroundJob]
  }
```

### StructuredValue

```haskell
data StructuredValue
  = SText T.Text
  | STable [T.Text] [[T.Text]]
```

---

## Execution Flow

1. **User Input:** `@ls /home | filter { .size > 1MB }`

2. **Parsing:** Tokenize and split into segments:

    ```haskell
    Pipeline [
      Segment Structured "ls" ["/home"],
      Segment Structured "filter" ["{", ".size", ">", "1MB", "}"]
    ]
    ```

3. **Alias Expansion:** Replace aliases with their definitions

4. **Execution:**

    - Raw Mode: Resolve in `$PATH`, fork process
    - Structured Mode: Look up in registry, return `StructuredValue`

5. **Pipeline Chaining:** Pass output between stages

---

## Built-in Commands

Handled directly in `runRawSingle`:

-   `cd`, `exit`, `export`, `unset`
-   `pwd`, `history`, `clear`
-   `source`, `type`, `which`, `echo`
-   `true`, `false`, `jobs`

---

## Structured Commands

| Command | Function         |
| ------- | ---------------- |
| `@ls`   | `lsStructured`   |
| `@ps`   | `psStructured`   |
| `@df`   | `dfStructured`   |
| `@env`  | `envStructured`  |
| `@find` | `findStructured` |

### Transformations

| Transform | Function           |
| --------- | ------------------ |
| `filter`  | `filterStructured` |
| `sort`    | `sortStructured`   |
| `select`  | `selectStructured` |

---

## Configuration

`ConfigFast.hs` uses simple key=value parsing (no external TOML library).
Config files use TOML-like syntax but are parsed with a lightweight internal parser:

```haskell
parseConfigLines :: [T.Text] -> ShellConfig
```

Supports:

-   Prompt settings
-   Colors
-   Aliases (`alias.name = "command"`)
-   Security options

---

## Performance

| Component           | Time     |
| ------------------- | -------- |
| Shell startup       | ~25ms    |
| Prompt generation   | ~10-50ms |
| Structured commands | ~1-10ms  |

---

## Design Principles

1. **Type Safety:** Haskell prevents runtime errors
2. **Explicit Modes:** `@` prefix makes structured mode clear
3. **Interoperability:** Raw mode works with all Unix tools
4. **Fast Startup:** Simple config, no interpreter overhead
