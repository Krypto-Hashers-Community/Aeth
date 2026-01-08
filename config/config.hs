-- move this file to $HOME/.config/Aeth to customize your shell
module Config where

import Control.Exception (IOException, try)
import Data.IORef
import Data.List (isSuffixOf)
import Data.Maybe (fromMaybe)
import Data.Time (getZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified System.Directory as Dir
import System.Environment (lookupEnv)
import qualified System.Exit as Exit
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Unistd (SystemID (nodeName), getSystemID)
import qualified System.Process as Proc

-- Cached values computed once at module load
username :: String
username = unsafePerformIO getUsername

hostname :: String
hostname = unsafePerformIO (nodeName <$> getSystemID)

-- Cache for lang icons based on cwd
cachedLangIcons :: IORef (Maybe (FilePath, [String]))
cachedLangIcons = unsafePerformIO (newIORef Nothing)

-- Optional: enable fullscreen TUI
useTui :: Bool
useTui = False

-- 1. Nerd Font icon constants (Safe Hex Escapes)
iconHaskell, iconFolder, iconGit, iconError, iconVim, iconLambda, iconDocker, iconSun :: String
iconHaskell = "\xE637" -- 
iconFolder = "\xF07B" -- 
iconGit = "\xF418" -- 
iconError = "\xF00D" -- 
iconVim = "\xE62B" -- 
iconLambda = "\x03BB" -- λ
iconDocker = "\xF308" -- 
iconSun = "\x2600" -- ☀

iconPython, iconNode, iconRust, iconGo, iconC :: String
iconPython = "\xF81F" -- 
iconNode = "\xE718" -- 
iconRust = "\xE7A8" -- 
iconGo = "\xE627" -- 
iconC = "\xE61E" -- 

-- 2. Helpers for Dynamic Data
getUsername :: IO String
getUsername = do
  m <- lookupEnv "USER"
  pure (fromMaybe "" m)

-- Note: the shell already passes a "pretty cwd" (e.g. ~/proj) as the first arg.

--
-- These are intentionally lightweight and do NOT require changes to the core shell.
-- If any detection is too slow on your machine, remove the process calls and keep only file checks.

safeReadFile :: FilePath -> IO (Maybe String)
safeReadFile p = do
  r <- try (readFile p) :: IO (Either IOException String)
  pure $ either (const Nothing) (Just . trim) r
  where
    trim = reverse . dropWhile (== '\n') . reverse

safeReadProcess :: String -> [String] -> IO (Maybe String)
safeReadProcess exe args = do
  r <- try (Proc.readProcessWithExitCode exe args "") :: IO (Either IOException (Exit.ExitCode, String, String))
  case r of
    Left _ -> pure Nothing
    Right (Exit.ExitSuccess, out, _) ->
      let t = takeWhile (/= '\n') out
       in pure (if null t then Nothing else Just t)
    Right _ -> pure Nothing

getPrimaryIp :: IO (Maybe String)
getPrimaryIp = do
  m <- safeReadProcess "hostname" ["-I"]
  pure (fmap (takeWhile (/= ' ')) m)

getBatteryPercent :: IO (Maybe String)
getBatteryPercent = do
  -- Laptops usually expose BAT0; desktops often have no battery.
  safeReadFile "/sys/class/power_supply/BAT0/capacity"

detectLangIcons :: IO [String]
detectLangIcons = do
  cwd <- Dir.getCurrentDirectory
  mCached <- readIORef cachedLangIcons
  case mCached of
    Just (cachedCwd, icons) | cachedCwd == cwd -> pure icons
    _ -> do
      entries <- Dir.listDirectory cwd
      let has f = Dir.doesFileExist (cwd <> "/" <> f)
      let anyHas fs = or <$> mapM has fs

      let hasCabal = any (isSuffixOf ".cabal") entries
      isHsSentinel <- anyHas ["cabal.project", "stack.yaml", "package.yaml"]
      let isHs = hasCabal || isHsSentinel
      isPy <- anyHas ["pyproject.toml", "requirements.txt", "Pipfile", "poetry.lock"]
      isNode <- anyHas ["package.json", "pnpm-lock.yaml", "yarn.lock", "package-lock.json"]
      isRust <- anyHas ["Cargo.toml"]
      isGo <- anyHas ["go.mod"]
      isC <- anyHas ["CMakeLists.txt", "Makefile"]

      let icons =
            [iconHaskell | isHs]
              ++ [iconPython | isPy]
              ++ [iconNode | isNode]
              ++ [iconRust | isRust]
              ++ [iconGo | isGo]
              ++ [iconC | isC]
      writeIORef cachedLangIcons (Just (cwd, icons))
      pure icons

-- 3. Prompt segments API
--
-- Format: [(Foreground, Background, Content)]
--
-- Args:
--   cwdPretty      :: FilePath        -- already home-shortened when possible
--   gitBranch      :: Maybe String
--   lastExitCode   :: Int
--   lastDurationMs :: Maybe Int       -- duration of the last command/pipeline
--
-- Background is set to "default" for a transparent look.
myPromptSegments :: FilePath -> Maybe String -> Int -> Maybe Int -> IO [(String, String, String)]
myPromptSegments cwdPretty gitBranch lastExit lastDurationMs = do
  now <- getZonedTime
  let clock = formatTime defaultTimeLocale "%H:%M" now
  mIp <- getPrimaryIp
  mBat <- getBatteryPercent
  langIcons <- detectLangIcons
  pure
    [ ("yellow", "default", clock ++ " "),
      ("cyan", "default", username ++ "@" ++ hostname ++ " "),
      ("blue", "default", iconFolder ++ " " ++ cwdPretty),
      gitSeg gitBranch,
      langsSeg langIcons,
      ipSeg mIp,
      batterySeg mBat,
      durationSeg lastDurationMs,
      (arrowColor lastExit, "default", " ~> ")
    ]
  where
    gitSeg (Just branch) = ("green", "default", " " ++ iconGit ++ " " ++ branch)
    gitSeg Nothing = ("gray", "default", "")

    durationSeg (Just ms)
      | ms >= 1000 = ("magenta", "default", " " ++ show (ms `div` 1000) ++ "s")
      | otherwise = ("magenta", "default", " " ++ show ms ++ "ms")
    durationSeg Nothing = ("gray", "default", "")

    langsSeg [] = ("gray", "default", "")
    langsSeg xs = ("magenta", "default", " " ++ unwords xs)

    ipSeg (Just ip) = ("gray", "default", " " ++ ip)
    ipSeg Nothing = ("gray", "default", "")

    batterySeg (Just pct) = ("gray", "default", " " ++ pct ++ "%")
    batterySeg Nothing = ("gray", "default", "")

    arrowColor 0 = "magenta" -- Success color
    arrowColor _ = "red" -- Failure color

-- 4. Structured Commands
getStructuredWeather = putStrLn (iconSun ++ "  24°C - Mostly Sunny")

getStructuredGitStatus = putStrLn (iconGit ++ " branch: main [ahead 1]")

getStructuredDocker = putStrLn (iconDocker ++ " Running: 3 | Stopped: 1")

-- 5. The Object Registry
myExtensions :: [(String, IO ())]
myExtensions =
  [ ("@weather", getStructuredWeather),
    ("@git", getStructuredGitStatus),
    ("@docker", getStructuredDocker)
  ]

-- Structured filtering (prototype)
--
-- You can filter a table produced by @ls using:
--   @ls /some/path | filter { .size > 1MB }
--   @ls | filter { .kind == dir }
--   @ls | filter { .name contains kde }
--
-- Supported fields from @ls: .name, .kind, .size (bytes)
-- Supported operators: ==, !=, >, >=, <, <=, contains