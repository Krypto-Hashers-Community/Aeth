{-# LANGUAGE OverloadedStrings #-}

module Aeth.Shell
  ( run,
    runCommandLine,
  )
where

import Aeth.Config
import Aeth.Exec
import Aeth.LineEditor
import qualified Aeth.LineEditorVty as VtyEd
import Aeth.Parse
import Aeth.Types
import Control.Exception (IOException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, evalStateT, get, modify')
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.FilePath as FP
import System.IO (hFlush, stderr, stdout)

run :: IO ()
run = do
  initialCwd <- Dir.getCurrentDirectory
  env0 <- Env.getEnvironment
  let st0 = emptyShellState initialCwd (Map.fromList env0)
  -- Load config for uiMode and prompt/extensions.
  (cfg, mCfgErr) <- loadConfig
  -- Install the prompt function once (avoids re-loading config.hs in initPrompt).
  setPromptFunction (prompt cfg)
  case mCfgErr of
    Nothing -> pure ()
    Just e -> TIO.hPutStrLn stderr ("Aeth: " <> T.pack e)
  evalStateT (startup cfg >> runLoop cfg) st0

runLoop :: ShellConfig -> StateT ShellState IO ()
runLoop cfg =
  case uiMode cfg of
    NormalUi -> loopNormal cfg
    TuiUi -> loopTui cfg

runCommandLine :: String -> IO ()
runCommandLine line = do
  initialCwd <- Dir.getCurrentDirectory
  env0 <- Env.getEnvironment
  let st0 = emptyShellState initialCwd (Map.fromList env0)
  evalStateT (runOne (T.pack line)) st0

loopNormal :: ShellConfig -> StateT ShellState IO ()
loopNormal _cfg =
  withLineEditor $ \ed -> go ed
  where
    go ed = do
      st <- get
      promptFn <- liftIO getPrompt
      promptStr <- liftIO (promptFn st)
      mLine <- liftIO (getLineEdited ed [] promptStr (history st))
      case mLine of
        Nothing -> pure ()
        Just line -> do
          liftIO (appendHistory line)
          modify' (\st' -> st' {history = history st' ++ [line]})
          runOne (T.pack line)
          go ed

loopTui :: ShellConfig -> StateT ShellState IO ()
loopTui _cfg = do
  st0 <- get
  _ <- liftIO $ VtyEd.withLineEditor $ \ed -> evalStateT (go ed []) st0
  pure ()
  where
    go :: VtyEd.LineEditor -> [T.Text] -> StateT ShellState IO ()
    go ed scrollback = do
      st <- get
      promptFn <- liftIO getPrompt
      promptStr <- liftIO (promptFn st)
      mLine <- liftIO (VtyEd.getLineEdited ed scrollback promptStr (history st))
      case mLine of
        Nothing -> pure ()
        Just line -> do
          liftIO (appendHistory line)
          modify' (\st' -> st' {history = history st' ++ [line]})
          out <- runOneCapture (T.pack line)
          let newScroll =
                trimScrollback 1000 (scrollback ++ [T.pack (promptStr <> line)] ++ nonEmptyLines out)
          go ed newScroll

runOneCapture :: T.Text -> StateT ShellState IO T.Text
runOneCapture t =
  case parsePipeline t of
    Left e ->
      if e == "empty"
        then pure ""
        else do
          modify' (\st -> st {lastExitCode = 2})
          modify' (\st -> st {lastDurationMs = Nothing})
          pure ("Aeth: parse: " <> renderParseError e)
    Right p -> do
      start <- liftIO getCurrentTime
      out <- runPipelineCapture p
      end <- liftIO getCurrentTime
      let ms = max 0 (floor (realToFrac (diffUTCTime end start) * (1000 :: Double)) :: Int)
      modify' (\st -> st {lastDurationMs = Just ms})
      pure out

nonEmptyLines :: T.Text -> [T.Text]
nonEmptyLines t =
  let ls = T.lines t
   in filter (not . T.null) ls

trimScrollback :: Int -> [T.Text] -> [T.Text]
trimScrollback maxN xs
  | maxN <= 0 = []
  | length xs <= maxN = xs
  | otherwise = drop (length xs - maxN) xs

startup :: ShellConfig -> StateT ShellState IO ()
startup _cfg = do
  -- Source a minimal rc file for "shell startup" behavior.
  -- This is intentionally line-based (no complex scripting yet).
  path <- liftIO rcFilePath
  exists <- liftIO (Dir.doesFileExist path)
  when exists $ do
    contents <- liftIO (TIO.readFile path)
    let ls = filter (not . T.null) (map stripComments (T.lines contents))
    mapM_ runOne ls
  -- Load history once at startup
  hist <- liftIO readHistory
  modify' (\st -> st {history = hist})
  where
    stripComments t = T.strip (T.takeWhile (/= '#') t)

runOne :: (MonadIO m) => T.Text -> StateT ShellState m ()
runOne t =
  case parsePipeline t of
    Left e ->
      if e == "empty"
        then pure ()
        else do
          modify' (\st -> st {lastExitCode = 2})
          modify' (\st -> st {lastDurationMs = Nothing})
          liftIO (TIO.hPutStrLn stderr ("Aeth: parse: " <> renderParseError e))
    Right p -> do
      start <- liftIO getCurrentTime
      liftIO (hFlush stdout)
      runPipeline Map.empty p
      end <- liftIO getCurrentTime
      let ms = max 0 (floor (realToFrac (diffUTCTime end start) * (1000 :: Double)) :: Int)
      modify' (\st -> st {lastDurationMs = Just ms})

readHistory :: IO [String]
readHistory = do
  path <- historyFilePath
  exists <- Dir.doesFileExist path
  if not exists
    then pure []
    else do
      result <- try (readFile path) :: IO (Either IOException String)
      case result of
        Left _ -> pure []
        Right s -> pure (lines s)

appendHistory :: String -> IO ()
appendHistory line = do
  when (not (null line)) $ do
    path <- historyFilePath
    Dir.createDirectoryIfMissing True (FP.takeDirectory path)
    _ <- try (appendFile path (line <> "\n")) :: IO (Either IOException ())
    pure ()
