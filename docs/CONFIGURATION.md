# Configuration Guide

This guide explains how to customize Aeth to match your workflow and preferences.

## Configuration File Location

Aeth looks for its configuration files at:

```
~/.config/Aeth/config.hs
~/.config/Aeth/rc
~/.config/Aeth/sequences.txt
```

A template is provided in the repository at `config/`. Copy it to get started:

```bash
mkdir -p ~/.config/Aeth
cp config/config.hs ~/.config/Aeth/config.hs
cp config/rc ~/.config/Aeth/rc
cp config/sequences.txt ~/.config/Aeth/sequences.txt
```

---

## The `rc` File

The `rc` file is an optional configuration resource for Aeth, located at:

```
~/.config/Aeth/rc
```

### Purpose

-   The `rc` file allows you to reference additional configuration resources or run startup commands when Aeth starts.
-   It is typically used to include files such as `sequences.txt` (for key bindings or terminal escape sequences), set environment variables, or run initialization commands.

### Default Example

The default `rc` file provided in the repository contains:

```sh
cat ~/.config/Aeth/sequences.txt
```

This means that, on startup, Aeth will read and apply the terminal escape sequences or key bindings defined in `sequences.txt`.

### Customization

-   You can edit `~/.config/Aeth/rc` to add your own commands or reference other files.
-   For example, to set an environment variable and include a custom key sequence file:

```sh
export MY_AETH_VAR=1
cat ~/.config/Aeth/sequences.txt
cat ~/.config/Aeth/my_custom_keys.txt
```

-   You can use any shell commands supported by your environment.

### Tips

-   If you do not need custom startup commands, you can leave the `rc` file as-is or remove it.
-   If you want to add more advanced startup logic, simply add the relevant shell commands to this file.

---

## Configuration File Structure

The config file is pure Haskell code, giving you maximum flexibility and type safety. The shell dynamically loads it at startup using the `hint` library.

### Basic Structure

```haskell
module Config where

import Control.Exception (IOException, try)
import Data.List (isSuffixOf)
import qualified System.Directory as Dir
import System.Environment (lookupEnv)
-- ... other imports

-- Main configuration options
useTui :: Bool
myPromptSegments :: FilePath -> Maybe String -> Int -> Maybe Int -> IO [(String, String, String)]
myExtensions :: [(String, IO ())]
```

---

## Configuration Options

### 1. UI Mode

Choose between normal line-based UI and fullscreen TUI:

```haskell
useTui :: Bool
useTui = False  -- Normal UI (default)
-- useTui = True   -- Fullscreen TUI with scrollback
```

**Normal UI:** Traditional REPL with readline-style editing (Haskeline)  
**TUI UI:** Fullscreen interface with scrollback buffer (experimental)

---

### 2. Prompt Customization

The prompt is defined by the `myPromptSegments` function, which returns a list of colored segments.

#### Function Signature

```haskell
myPromptSegments :: FilePath       -- Current directory (home-shortened)
                 -> Maybe String   -- Git branch (if in git repo)
                 -> Int            -- Last exit code
                 -> Maybe Int      -- Last command duration (ms)
                 -> IO [(String, String, String)]
```

Each tuple in the result list represents:

1. **Foreground color** (e.g., "red", "green", "blue", "yellow", "cyan", "magenta", "white", "gray")
2. **Background color** (use "default" for transparency)
3. **Content** (the text to display)

#### Default Prompt

The default configuration shows:

```haskell
myPromptSegments cwdPretty gitBranch lastExit lastDurationMs = do
  username <- getUsername
  hostname <- nodeName <$> getSystemID
  now <- getZonedTime
  let clock = formatTime defaultTimeLocale "%H:%M" now
  mIp <- getPrimaryIp
  mBat <- getBatteryPercent
  langIcons <- detectLangIcons

  pure [
    ("yellow",  "default", clock ++ " "),
    ("cyan",    "default", username ++ "@" ++ hostname ++ " "),
    ("blue",    "default", iconFolder ++ " " ++ cwdPretty),
    gitSeg gitBranch,
    langsSeg langIcons,
    ipSeg mIp,
    batterySeg mBat,
    durationSeg lastDurationMs,
    (arrowColor lastExit, "default", " ~> ")
  ]
```

#### Customization Examples

**Minimal prompt:**

```haskell
myPromptSegments cwdPretty _ lastExit _ =
  pure [("green", "default", cwdPretty ++ " $ ")]
```

**Lambda prompt (classic Haskell):**

```haskell
myPromptSegments cwdPretty _ lastExit _ = do
  let color = if lastExit == 0 then "green" else "red"
  pure [
    ("blue", "default", cwdPretty ++ " "),
    (color, "default", "λ> ")
  ]
```

**Powerline-style:**

```haskell
myPromptSegments cwdPretty gitBranch lastExit _ = do
  username <- getUsername
  pure [
    ("white", "blue",  " " ++ username ++ " "),
    ("blue",  "green", ""),  -- Powerline separator
    ("black", "green", " " ++ cwdPretty ++ " "),
    gitSegment gitBranch,
    (exitColor lastExit, "default", " ❯ ")
  ]
```

---

### 3. Nerd Font Icons

The config provides icon constants using Unicode hex escapes:

```haskell
iconHaskell = "\xE637"  --
iconFolder  = "\xF07B"  --
iconGit     = "\xF418"  --
iconPython  = "\xF81F"  --
iconNode    = "\xE718"  --
iconRust    = "\xE7A8"  --
iconDocker  = "\xF308"  --
-- ... and more
```

You can add your own icons by finding their Unicode codepoints in your Nerd Font documentation.

---

### 4. Dynamic Context Detection

The config includes helper functions to detect project context:

#### Language/Framework Detection

```haskell
detectLangIcons :: IO [String]
```

Automatically detects project types by checking for sentinel files:

-   **Haskell:** `.cabal`, `cabal.project`, `stack.yaml`, `package.yaml`
-   **Python:** `pyproject.toml`, `requirements.txt`, `Pipfile`, `poetry.lock`
-   **Node:** `package.json`, `pnpm-lock.yaml`, `yarn.lock`
-   **Rust:** `Cargo.toml`
-   **Go:** `go.mod`
-   **C/C++:** `CMakeLists.txt`, `Makefile`

Customize by modifying the `detectLangIcons` function.

#### Git Branch Detection

Handled automatically by the shell core. Returns `Maybe String`:

-   `Just "main"` if in a git repo
-   `Nothing` if not in a repo or git unavailable

#### System Information

```haskell
getUsername :: IO String         -- Current user
getPrimaryIp :: IO (Maybe String) -- Primary network IP
getBatteryPercent :: IO (Maybe String)  -- Battery % (laptops only)
```

---

### 5. Custom Structured Commands

Add your own structured commands to the registry:

```haskell
myExtensions :: [(String, IO ())]
myExtensions = [
  ("@weather", getStructuredWeather),
  ("@git",     getStructuredGitStatus),
  ("@docker",  getStructuredDocker),
  ("@time",    getStructuredTime)  -- Your custom command
]

getStructuredTime :: IO ()
getStructuredTime = do
  now <- getCurrentTime
  putStrLn $ "Current time: " ++ show now
```

**Note:** These are currently basic IO actions. Full structured data type support (returning `StructuredValue`) is coming in future versions.

---

## Advanced Configuration

### Colors

Supported color names:

-   `"black"`, `"red"`, `"green"`, `"yellow"`, `"blue"`, `"magenta"`, `"cyan"`, `"white"`, `"gray"`
-   `"default"` (transparent background, inherits terminal colors)

### Safe File/Process Reading

The config provides safe helpers that handle errors gracefully:

```haskell
safeReadFile :: FilePath -> IO (Maybe String)
safeReadProcess :: String -> [String] -> IO (Maybe String)
```

Use these when adding custom prompt segments that read from files or processes.

---

## Filtering Configuration

Structured filtering supports:

**Operators:** `==`, `!=`, `>`, `>=`, `<`, `<=`, `contains`  
**Fields (from `@ls`):** `.name`, `.kind`, `.size`

Example filters:

```bash
@ls | filter { .size > 1MB }
@ls | filter { .kind == dir }
@ls | filter { .name contains config }
```

Size units: `B`, `KB`, `MB`, `GB` (case-insensitive)

---

## Troubleshooting

### Config not loading

1. Check file location: `~/.config/Aeth/config.hs`
2. Verify it's valid Haskell (shell shows parse errors on startup)
3. Ensure all imports are available (the template uses only base libraries)

### Icons not showing

1. Install a Nerd Font: https://www.nerdfonts.com/
2. Configure your terminal to use it
3. Test with: `echo "\xE637"` (should show )

### Slow prompt

Dynamic detection (git, language icons, IP, battery) can add latency. Remove expensive operations:

```haskell
myPromptSegments cwdPretty _ lastExit _ =
  -- Skip all IO operations for speed
  pure [("green", "default", cwdPretty ++ " $ ")]
```

---

## Example Configurations

### Minimalist

```haskell
useTui = False

myPromptSegments cwdPretty _ lastExit _ =
  pure [("blue", "default", cwdPretty ++ " λ ")]

myExtensions = []
```

### Feature-Rich (Default)

See `config/config.hs` in the repository.

### Powerline-Inspired

```haskell
myPromptSegments cwdPretty gitBranch lastExit _ = do
  user <- getUsername
  host <- nodeName <$> getSystemID

  pure [
    ("white",  "blue",   " " ++ user ++ "@" ++ host ++ " "),
    ("blue",   "green",  ""),
    ("black",  "green",  " " ++ cwdPretty ++ " "),
    gitSegment gitBranch,
    (exitColor lastExit, "default", " ❯ ")
  ]
  where
    gitSegment (Just br) = ("white", "magenta", " " ++ iconGit ++ " " ++ br ++ " ")
    gitSegment Nothing   = ("gray",  "default",  "")
    exitColor 0 = "green"
    exitColor _ = "red"
```

---

## Next Steps

-   See [ARCHITECTURE.md](ARCHITECTURE.md) for understanding the shell internals
-   Experiment with the template config
-   Share your custom configs with the community!
