{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Main shell module - improved version with haskeline.
--
-- Key improvements:
-- - Uses haskeline for proper PTY handling (AI/Copilot compatible)
-- - Fast startup (no hint/GHC interpreter)
-- - Proper signal handling
-- - Tab completion
-- - Terminal state properly restored on exit
module Aeth.ShellFast
  ( run,
    runCommandLine,
  )
where

import Aeth.ConfigFast
import Aeth.Exec
import Aeth.Parse
import Aeth.Types
import Control.Exception (IOException, catch)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, evalStateT, modify', runStateT)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified System.Console.Haskeline as HL
import qualified System.Console.Haskeline.Completion as HLC
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.FilePath as FP
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Signals (Handler (..), installHandler, sigINT, sigTSTP)

-- | Global state reference for shell state (needed for completion)
{-# NOINLINE globalStateRef #-}
globalStateRef :: IORef ShellState
globalStateRef = unsafePerformIO $ newIORef (emptyShellState "/" Map.empty)

-- | Main entry point
run :: IO ()
run = do
  -- Install signal handlers
  installSignalHandlers

  -- Initialize state
  initialCwd <- Dir.getCurrentDirectory
  env0 <- Env.getEnvironment
  let st0 = emptyShellState initialCwd (Map.fromList env0)
  writeIORef globalStateRef st0

  -- Load config (fast - no hint!)
  (cfg, mCfgErr) <- loadConfig
  case mCfgErr of
    Nothing -> pure ()
    Just e -> TIO.hPutStrLn stderr ("Aeth: " <> T.pack e)

  -- Run the shell with haskeline
  histPath <- historyFilePath
  let hlSettings = makeHaskelineSettings cfg histPath
  HL.runInputT hlSettings $ do
    -- Source rc file
    liftIO $ runStartup cfg st0
    -- Enter main loop
    mainLoop cfg

-- | Run a single command (for -c flag)
runCommandLine :: String -> IO ()
runCommandLine line = do
  initialCwd <- Dir.getCurrentDirectory
  env0 <- Env.getEnvironment
  let st0 = emptyShellState initialCwd (Map.fromList env0)
  (cfg, errs) <- loadConfig
  -- Surface config errors to stderr
  case errs of
    Just err -> TIO.hPutStrLn stderr ("Aeth: config: " <> T.pack err)
    Nothing -> pure ()
  evalStateT (runOne cfg (T.pack line)) st0

-- | Install signal handlers
installSignalHandlers :: IO ()
installSignalHandlers = do
  -- SIGINT (Ctrl+C) - just ignore it, let haskeline handle it
  _ <- installHandler sigINT (Catch (return ())) Nothing
  -- SIGTSTP (Ctrl+Z) - ignore for now
  _ <- installHandler sigTSTP (Catch (return ())) Nothing
  return ()

-- | Create haskeline settings with completion
makeHaskelineSettings :: ShellConfig -> FilePath -> HL.Settings IO
makeHaskelineSettings _cfg histPath =
  HL.Settings
    { HL.complete = shellCompleter,
      HL.historyFile = Just histPath, -- Enable haskeline history for up-arrow
      HL.autoAddHistory = True -- Let haskeline handle history
    }

-- | Tab completer for shell commands
shellCompleter :: HL.CompletionFunc IO
shellCompleter = HLC.completeWordWithPrev Nothing " \t|;" completionFunction

completionFunction :: String -> String -> IO [HL.Completion]
completionFunction _leftOfCursor wordToComplete = do
  st <- readIORef globalStateRef
  let currentDir = cwd st

  -- Check if it looks like a path
  if '/' `elem` wordToComplete || "." `elem` [take 1 wordToComplete, take 2 wordToComplete]
    then completeFilePath currentDir wordToComplete
    else do
      -- Complete both commands and files
      fileCmps <- completeFilePath currentDir wordToComplete
      cmdCmps <- completeCommand wordToComplete
      return $ cmdCmps ++ fileCmps

-- | Complete file paths
completeFilePath :: FilePath -> String -> IO [HL.Completion]
completeFilePath basedir prefix = do
  let (dir, filePrefix) = FP.splitFileName prefix
      searchDir =
        if null dir
          then basedir
          else
            if FP.isAbsolute dir
              then dir
              else basedir FP.</> dir

  dirExists <- Dir.doesDirectoryExist searchDir
  if not dirExists
    then return []
    else do
      contents <- safeListDirectory searchDir
      let matches = filter (matchesPrefix filePrefix) contents
      mapM (makeFileCompletion dir searchDir) (take 50 matches) -- Limit results
  where
    matchesPrefix pref name =
      map lowerChar pref `prefixMatches` map lowerChar name
    lowerChar c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
    prefixMatches [] _ = True
    prefixMatches _ [] = False
    prefixMatches (x : xs) (y : ys) = x == y && prefixMatches xs ys

safeListDirectory :: FilePath -> IO [String]
safeListDirectory dir = Dir.listDirectory dir `catch` (\(_ :: IOException) -> return [])

makeFileCompletion :: String -> FilePath -> String -> IO HL.Completion
makeFileCompletion prefix searchDir name = do
  let fullPath = searchDir FP.</> name
  isDir <- Dir.doesDirectoryExist fullPath
  let displayName = prefix ++ name
      replacement = if isDir then displayName ++ "/" else displayName
  return $ HL.Completion replacement name (not isDir)

-- | Complete commands
completeCommand :: String -> IO [HL.Completion]
completeCommand prefix = do
  -- Builtins
  let builtins = ["cd", "exit", "export", "pwd", "history", "clear", "alias", "unset", "source"]
      builtinMatches =
        [ HL.Completion cmd cmd True
          | cmd <- builtins,
            prefix `isPrefixOf` cmd
        ]

  -- PATH commands
  pathEnv <- Env.lookupEnv "PATH"
  let pathDirs = maybe [] FP.splitSearchPath pathEnv
  exeMatches <- fmap concat $ mapM (findExecutablesIn prefix) (take 10 pathDirs) -- Limit dirs searched
  return $ builtinMatches ++ take 50 exeMatches

findExecutablesIn :: String -> FilePath -> IO [HL.Completion]
findExecutablesIn prefix dir = do
  exists <- Dir.doesDirectoryExist dir
  if not exists
    then return []
    else do
      contents <- safeListDirectory dir
      let matches = filter (prefix `isPrefixOf`) contents
      return
        [ HL.Completion name name True
          | name <- take 20 matches
        ]

-- | Main loop with haskeline
mainLoop :: ShellConfig -> HL.InputT IO ()
mainLoop cfg = go
  where
    go = do
      -- Get current state
      st <- liftIO $ readIORef globalStateRef

      -- Generate prompt
      promptStr <- liftIO $ mkPromptFunction cfg st

      -- Get input
      mInput <- HL.getInputLine promptStr
      case mInput of
        Nothing -> return () -- EOF (Ctrl+D)
        Just "" -> go -- Empty line
        Just line -> do
          -- Update internal history for shell access
          liftIO $ modifyIORef' globalStateRef (\s -> s {history = history s ++ [line]})

          -- Execute
          liftIO $ runLineWithState line cfg
          go

-- | Run a line and update global state
runLineWithState :: String -> ShellConfig -> IO ()
runLineWithState line cfg = do
  st <- readIORef globalStateRef
  ((), newSt) <- runStateT (runOne cfg (T.pack line)) st
  writeIORef globalStateRef newSt

-- | Run startup (rc file, history)
runStartup :: ShellConfig -> ShellState -> IO ()
runStartup cfg st0 = do
  writeIORef globalStateRef st0

  -- Source rc file
  path <- rcFilePath
  exists <- Dir.doesFileExist path
  when exists $ do
    contents <- TIO.readFile path
    let ls = filter (not . T.null) (map stripComments (T.lines contents))
    mapM_ (\l -> runLineWithState (T.unpack l) cfg) ls

  -- Load history
  hist <- readHistory
  modifyIORef' globalStateRef (\s -> s {history = hist})
  where
    stripComments t = T.strip (T.takeWhile (/= '#') t)

-- | Execute a single command/pipeline
runOne :: (MonadIO m) => ShellConfig -> T.Text -> StateT ShellState m ()
runOne cfg t =
  case parsePipeline t of
    Left e ->
      if e == "empty"
        then pure ()
        else do
          modify' (\st -> st {lastExitCode = 2, lastDurationMs = Nothing})
          liftIO $ TIO.hPutStrLn stderr ("Aeth: parse: " <> T.pack e)
    Right p -> do
      start <- liftIO getCurrentTime
      runPipeline (aliases cfg) p
      end <- liftIO getCurrentTime
      let ms = max 0 (floor (realToFrac (diffUTCTime end start) * (1000 :: Double)) :: Int)
      modify' (\st -> st {lastDurationMs = Just ms})
