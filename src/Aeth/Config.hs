{-# LANGUAGE OverloadedStrings #-}

module Aeth.Config
  ( ShellConfig (..),
    UiMode (..),
    PromptSegment (..),
    renderPromptSegments,
    defaultConfig,
    loadConfig,
    initPrompt,
    getPrompt,
    configDir,
    configHsPath,
    configTomlPath,
    rcFilePath,
    historyFilePath,
  )
where

import Aeth.Structured (StructuredValue (..))
import Aeth.Types (ShellState (..))
import Control.Concurrent (forkIO)
import Control.Exception (IOException, try)
import Data.Char (isAsciiUpper, isSpace, toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Language.Haskell.Interpreter as Hint
import qualified System.Console.ANSI as ANSI
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import System.IO.Unsafe (unsafePerformIO)
import qualified System.Posix.DynamicLinker as DL
import qualified System.Process as Proc
import Toml (TomlCodec, decode, parse, (.=))
import qualified Toml

-- Minimal, safe surface area for user customization.
-- Keep this small while the shell is still experimental.

data ShellConfig = ShellConfig
  { prompt :: ShellState -> IO String,
    uiMode :: UiMode,
    structuredExtensions :: [(T.Text, [T.Text] -> ShellState -> IO StructuredValue)]
  }

-- | A small typed prompt building block. Intended for config.hs.
--
-- Color names are intentionally simple strings ("red", "blue", "gray", ...)
-- to keep the config surface area stable while the shell is experimental.
data PromptSegment = PromptSegment
  { foreground :: String,
    background :: String,
    content :: String
  }
  deriving (Eq, Show)

data ColorSpec
  = DefaultColor
  | NamedColor ANSI.ColorIntensity ANSI.Color

renderPromptSegments :: [PromptSegment] -> String
renderPromptSegments segs = ANSI.setSGRCode [ANSI.Reset] <> concatMap renderOne segs <> ANSI.setSGRCode [ANSI.Reset]
  where
    renderOne (PromptSegment fg bg txt)
      | null txt = ""
      | otherwise =
          let pre = ANSI.setSGRCode (fgSgr fg <> bgSgr bg)
           in pre <> txt

    fgSgr s =
      case parseColorSpec s of
        Nothing -> []
        Just DefaultColor -> [ANSI.SetDefaultColor ANSI.Foreground]
        Just (NamedColor intensity c) -> [ANSI.SetColor ANSI.Foreground intensity c]
    bgSgr s =
      case parseColorSpec s of
        Nothing -> []
        Just DefaultColor -> [ANSI.SetDefaultColor ANSI.Background]
        Just (NamedColor _intensity c) -> [ANSI.SetColor ANSI.Background ANSI.Dull c]

    parseColorSpec s0 =
      case map (\c -> if isAsciiUpper c then toLower c else c) s0 of
        "default" -> Just DefaultColor
        "black" -> Just (NamedColor ANSI.Vivid ANSI.Black)
        "red" -> Just (NamedColor ANSI.Vivid ANSI.Red)
        "green" -> Just (NamedColor ANSI.Vivid ANSI.Green)
        "yellow" -> Just (NamedColor ANSI.Vivid ANSI.Yellow)
        "blue" -> Just (NamedColor ANSI.Vivid ANSI.Blue)
        "magenta" -> Just (NamedColor ANSI.Vivid ANSI.Magenta)
        "cyan" -> Just (NamedColor ANSI.Vivid ANSI.Cyan)
        "white" -> Just (NamedColor ANSI.Vivid ANSI.White)
        "gray" -> Just (NamedColor ANSI.Dull ANSI.White)
        "grey" -> Just (NamedColor ANSI.Dull ANSI.White)
        _ -> Nothing

renderPromptTriples :: [(String, String, String)] -> String
renderPromptTriples triples =
  renderPromptSegments [PromptSegment fg bg txt | (fg, bg, txt) <- triples]

data UiMode = NormalUi | TuiUi
  deriving (Eq, Show)

data TomlConfig = TomlConfig
  { configUiMode :: UiMode,
    useDynamicPrompt :: Bool
  }
  deriving (Eq, Show)

uiModeToText :: UiMode -> T.Text
uiModeToText NormalUi = "normal"
uiModeToText TuiUi = "tui"

uiModeFromText :: T.Text -> UiMode
uiModeFromText "normal" = NormalUi
uiModeFromText "tui" = TuiUi
uiModeFromText t = error ("invalid ui_mode: " <> T.unpack t)

uiModeCodec :: TomlCodec UiMode
uiModeCodec = Toml.dimap uiModeToText uiModeFromText (Toml.text "ui_mode")

tomlConfigCodec :: TomlCodec TomlConfig
tomlConfigCodec =
  Toml.table
    ( TomlConfig
        <$> (Toml.dimap uiModeToText uiModeFromText (Toml.text "ui_mode") .= configUiMode)
        <*> (Toml.bool "use_dynamic_prompt" .= useDynamicPrompt)
    )
    "aeth"

defaultTomlConfig :: TomlConfig
defaultTomlConfig = TomlConfig NormalUi False

defaultConfig :: ShellConfig
defaultConfig =
  ShellConfig
    { prompt = \st -> pure (cwd st <> " > "),
      uiMode = NormalUi,
      structuredExtensions = []
    }

loadTomlConfig :: IO (TomlConfig, Maybe String)
loadTomlConfig = do
  path <- configTomlPath
  exists <- Dir.doesFileExist path
  if not exists
    then pure (defaultTomlConfig, Nothing)
    else do
      res <- Toml.decodeFileEither tomlConfigCodec path
      case res of
        Left errs -> pure (defaultTomlConfig, Just ("TOML decode error: " <> show errs))
        Right cfg -> pure (cfg, Nothing)

combineErrors :: Maybe String -> Maybe String -> Maybe String
combineErrors Nothing Nothing = Nothing
combineErrors (Just a) Nothing = Just a
combineErrors Nothing (Just b) = Just b
combineErrors (Just a) (Just b) = Just (a <> "; " <> b)

-- | Load combined configuration: TOML + optional Haskell `config.hs`.
loadConfig :: IO (ShellConfig, Maybe String)
loadConfig = do
  (tCfg, tErr) <- loadTomlConfig
  hsPath <- configHsPath
  cfgOrDefault <-
    if useDynamicPrompt tCfg
      then do
        exists <- Dir.doesFileExist hsPath
        if not exists
          then pure (defaultConfig, Nothing)
          else loadHaskellConfig hsPath
      else pure (defaultConfig, Nothing)
  let (hsCfg, hsErr) = cfgOrDefault
      finalCfg = hsCfg {uiMode = configUiMode tCfg}
      errs = combineErrors tErr hsErr
  pure (finalCfg, errs)

-- | Loads config from TOML and optionally Haskell.
--
-- If use_dynamic_prompt is true in config.toml, loads config.hs for prompt.
-- Otherwise, uses default prompt.

-- | Global IORef for the prompt function, initialized at startup
promptRef :: IORef (ShellState -> IO String)
promptRef = unsafePerformIO (newIORef (prompt defaultConfig))
{-# NOINLINE promptRef #-}

-- | Checks if config.so exists and is newer than config.hs
configSoUpToDate :: IO Bool
configSoUpToDate = do
  hsPath <- configHsPath
  soPath <- configSoPath
  hsExists <- Dir.doesFileExist hsPath
  soExists <- Dir.doesFileExist soPath
  if not (hsExists && soExists)
    then pure False
    else do
      hsTime <- Dir.getModificationTime hsPath
      soTime <- Dir.getModificationTime soPath
      pure (soTime > hsTime)

configSoPath :: IO FilePath
configSoPath = do
  dir <- configDir
  pure (dir FP.</> "config.so")

-- | Loads the prompt function from config.so using dynamic linker
loadPromptFromSo :: FilePath -> IO (Maybe (ShellState -> IO String))
loadPromptFromSo soPath = do
  -- This assumes the .so exports a symbol "aeth_prompt" of type ShellState -> IO String
  -- You must ensure ABI compatibility and symbol name during compilation
  -- This is a stub: actual implementation will require FFI and stable ABI
  pure Nothing -- TODO: Implement FFI loading

-- | Compiles config.hs to config.so in a background thread
compileConfigSo :: IO ()
compileConfigSo = do
  hsPath <- configHsPath
  soPath <- configSoPath
  -- Use the same GHC as main binary, with -shared -dynamic
  _ <- forkIO $ do
    _ <- Proc.readProcess "ghc" ["-shared", "-dynamic", hsPath, "-o", soPath] ""
    -- After compilation, attempt to load the new prompt function
    mPrompt <- loadPromptFromSo soPath
    case mPrompt of
      Just fn -> writeIORef promptRef fn
      Nothing -> pure ()
  pure ()

-- | Initializes the prompt function at shell startup
initPrompt :: IO ()
initPrompt = do
  upToDate <- configSoUpToDate
  soPath <- configSoPath
  if upToDate
    then do
      mPrompt <- loadPromptFromSo soPath
      case mPrompt of
        Just fn -> writeIORef promptRef fn
        Nothing -> fallback
    else fallback
  where
    fallback = do
      -- Use hint to interpret config.hs, or default prompt
      hsPath <- configHsPath
      exists <- Dir.doesFileExist hsPath
      if not exists
        then writeIORef promptRef (prompt defaultConfig)
        else do
          result <- try (loadHaskellConfig hsPath) :: IO (Either IOException (ShellConfig, Maybe String))
          case result of
            Right (cfg, _) -> writeIORef promptRef (prompt cfg)
            _ -> writeIORef promptRef (prompt defaultConfig)
      -- Start background compilation
      compileConfigSo

-- | Use this function to get the current prompt function
getPrompt :: IO (ShellState -> IO String)
getPrompt = readIORef promptRef

configDir :: IO FilePath
configDir = do
  mXdg <- Env.lookupEnv "XDG_CONFIG_HOME"
  case mXdg of
    Just xdg | not (null xdg) -> pure (xdg FP.</> "Aeth")
    _ -> do
      mHome <- Env.lookupEnv "HOME"
      let home = fromMaybe "." mHome
      pure (home FP.</> ".config" FP.</> "Aeth")

configHsPath :: IO FilePath
configHsPath = do
  dir <- configDir
  pure (dir FP.</> "config.hs")

configTomlPath :: IO FilePath
configTomlPath = do
  dir <- configDir
  pure (dir FP.</> "config.toml")

rcFilePath :: IO FilePath
rcFilePath = do
  dir <- configDir
  pure (dir FP.</> "rc")

historyFilePath :: IO FilePath
historyFilePath = do
  dir <- configDir
  pure (dir FP.</> "history")

loadHaskellConfig :: FilePath -> IO (ShellConfig, Maybe String)
loadHaskellConfig filePath = do
  -- The user module can just be `module Config where`.
  -- We interpret its exported values with explicit types.
  let run :: Hint.Interpreter a -> IO (Either Hint.InterpreterError a)
      run = Hint.runInterpreter

  loaded <- run $ do
    Hint.loadModules [filePath]
    Hint.setTopLevelModules ["Config"]
    -- Keep the interpreter context minimal so user config doesn't need access
    -- to the Aeth source tree (and so it works when installed).
    Hint.setImports ["Prelude"]

    promptFn <- do
      mSegments <- Hint.typeChecks "myPromptSegments"
      if mSegments
        then do
          ty <- Hint.typeOf "myPromptSegments"
          let norm = filter (not . isSpace) ty
          case norm of
            "FilePath->MaybeString->Int->MaybeInt->[(String,String,String)]" -> do
              fn <- Hint.interpret "myPromptSegments" (Hint.as :: FilePath -> Maybe String -> Int -> Maybe Int -> [(String, String, String)])
              pure
                ( \st -> do
                    (c, b, e, d) <- mkPromptArgsIO st
                    pure (renderPromptTriples (fn c b e d))
                )
            "FilePath->MaybeString->Int->MaybeInt->[((String,String,String))]" -> do
              fn <- Hint.interpret "myPromptSegments" (Hint.as :: FilePath -> Maybe String -> Int -> Maybe Int -> [(String, String, String)])
              pure
                ( \st -> do
                    (c, b, e, d) <- mkPromptArgsIO st
                    pure (renderPromptTriples (fn c b e d))
                )
            "FilePath->MaybeString->Int->MaybeInt->IO[(String,String,String)]" -> do
              fn <- Hint.interpret "myPromptSegments" (Hint.as :: FilePath -> Maybe String -> Int -> Maybe Int -> IO [(String, String, String)])
              pure
                ( \st -> do
                    (c, b, e, d) <- mkPromptArgsIO st
                    renderPromptTriples <$> fn c b e d
                )
            "FilePath->MaybeString->Int->MaybeInt->IO[((String,String,String))]" -> do
              fn <- Hint.interpret "myPromptSegments" (Hint.as :: FilePath -> Maybe String -> Int -> Maybe Int -> IO [(String, String, String)])
              pure
                ( \st -> do
                    (c, b, e, d) <- mkPromptArgsIO st
                    renderPromptTriples <$> fn c b e d
                )
            "FilePath->MaybeString->Int->[(String,String,String)]" -> do
              fn <- Hint.interpret "myPromptSegments" (Hint.as :: FilePath -> Maybe String -> Int -> [(String, String, String)])
              pure
                ( \st -> do
                    (c, b, e, _d) <- mkPromptArgsIO st
                    pure (renderPromptTriples (fn c b e))
                )
            "FilePath->MaybeString->Int->[((String,String,String))]" -> do
              fn <- Hint.interpret "myPromptSegments" (Hint.as :: FilePath -> Maybe String -> Int -> [(String, String, String)])
              pure
                ( \st -> do
                    (c, b, e, _d) <- mkPromptArgsIO st
                    pure (renderPromptTriples (fn c b e))
                )
            "FilePath->MaybeString->Int->IO[(String,String,String)]" -> do
              fn <- Hint.interpret "myPromptSegments" (Hint.as :: FilePath -> Maybe String -> Int -> IO [(String, String, String)])
              pure
                ( \st -> do
                    (c, b, e, _d) <- mkPromptArgsIO st
                    renderPromptTriples <$> fn c b e
                )
            "FilePath->MaybeString->Int->IO[((String,String,String))]" -> do
              fn <- Hint.interpret "myPromptSegments" (Hint.as :: FilePath -> Maybe String -> Int -> IO [(String, String, String)])
              pure
                ( \st -> do
                    (c, b, e, _d) <- mkPromptArgsIO st
                    renderPromptTriples <$> fn c b e
                )
            _ ->
              pure (\_ -> pure ("Aeth: unsupported myPromptSegments type: " <> ty <> "\n"))
        else do
          mPrompt <- Hint.typeChecks "myPrompt"
          if not mPrompt
            then pure (prompt defaultConfig)
            else do
              ty <- Hint.typeOf "myPrompt"
              let norm = filter (not . isSpace) ty
              case norm of
                "FilePath->Int->IOString" -> do
                  fn <- Hint.interpret "myPrompt" (Hint.as :: FilePath -> Int -> IO String)
                  pure
                    ( \st -> do
                        (c, _b, e, _d) <- mkPromptArgsIO st
                        fn c e
                    )
                "FilePath->Int->IO[Char]" -> do
                  fn <- Hint.interpret "myPrompt" (Hint.as :: FilePath -> Int -> IO String)
                  pure
                    ( \st -> do
                        (c, _b, e, _d) <- mkPromptArgsIO st
                        fn c e
                    )
                "FilePath->Int->String" -> do
                  fn <- Hint.interpret "myPrompt" (Hint.as :: FilePath -> Int -> String)
                  pure
                    ( \st -> do
                        (c, _b, e, _d) <- mkPromptArgsIO st
                        pure (fn c e)
                    )
                "FilePath->Int->[Char]" -> do
                  fn <- Hint.interpret "myPrompt" (Hint.as :: FilePath -> Int -> String)
                  pure
                    ( \st -> do
                        (c, _b, e, _d) <- mkPromptArgsIO st
                        pure (fn c e)
                    )
                "FilePath->MaybeString->Int->MaybeInt->String" -> do
                  fn <- Hint.interpret "myPrompt" (Hint.as :: FilePath -> Maybe String -> Int -> Maybe Int -> String)
                  pure
                    ( \st -> do
                        (c, b, e, d) <- mkPromptArgsIO st
                        pure (fn c b e d)
                    )
                "FilePath->MaybeString->Int->MaybeInt->[Char]" -> do
                  fn <- Hint.interpret "myPrompt" (Hint.as :: FilePath -> Maybe String -> Int -> Maybe Int -> String)
                  pure
                    ( \st -> do
                        (c, b, e, d) <- mkPromptArgsIO st
                        pure (fn c b e d)
                    )
                "FilePath->MaybeString->Int->MaybeInt->IOString" -> do
                  fn <- Hint.interpret "myPrompt" (Hint.as :: FilePath -> Maybe String -> Int -> Maybe Int -> IO String)
                  pure
                    ( \st -> do
                        (c, b, e, d) <- mkPromptArgsIO st
                        fn c b e d
                    )
                "FilePath->MaybeString->Int->MaybeInt->IO[Char]" -> do
                  fn <- Hint.interpret "myPrompt" (Hint.as :: FilePath -> Maybe String -> Int -> Maybe Int -> IO String)
                  pure
                    ( \st -> do
                        (c, b, e, d) <- mkPromptArgsIO st
                        fn c b e d
                    )
                "FilePath->MaybeString->Int->String" -> do
                  fn <- Hint.interpret "myPrompt" (Hint.as :: FilePath -> Maybe String -> Int -> String)
                  pure
                    ( \st -> do
                        (c, b, e, _d) <- mkPromptArgsIO st
                        pure (fn c b e)
                    )
                "FilePath->MaybeString->Int->[Char]" -> do
                  fn <- Hint.interpret "myPrompt" (Hint.as :: FilePath -> Maybe String -> Int -> String)
                  pure
                    ( \st -> do
                        (c, b, e, _d) <- mkPromptArgsIO st
                        pure (fn c b e)
                    )
                "FilePath->MaybeString->Int->IOString" -> do
                  fn <- Hint.interpret "myPrompt" (Hint.as :: FilePath -> Maybe String -> Int -> IO String)
                  pure
                    ( \st -> do
                        (c, b, e, _d) <- mkPromptArgsIO st
                        fn c b e
                    )
                "FilePath->MaybeString->Int->IO[Char]" -> do
                  fn <- Hint.interpret "myPrompt" (Hint.as :: FilePath -> Maybe String -> Int -> IO String)
                  pure
                    ( \st -> do
                        (c, b, e, _d) <- mkPromptArgsIO st
                        fn c b e
                    )
                _ ->
                  pure (\_ -> pure ("Aeth: unsupported myPrompt type: " <> ty <> "\n"))

    mUseTui <- Hint.typeChecks "useTui"
    useTuiVal <-
      if mUseTui
        then Hint.interpret "useTui" (Hint.as :: Bool)
        else pure False
    -- uiMode is now from TOML, ignore useTui here

    pure ShellConfig {prompt = promptFn, uiMode = NormalUi, structuredExtensions = []}

  case loaded of
    Left e -> pure (defaultConfig, Just (renderInterpreterError e))
    Right cfg -> pure (cfg, Nothing)

renderInterpreterError :: Hint.InterpreterError -> String
renderInterpreterError e =
  case e of
    Hint.WontCompile errs -> unlines (map Hint.errMsg errs)
    Hint.UnknownError s -> s
    Hint.NotAllowed s -> s
    Hint.GhcException s -> s

shortenHomePath :: FilePath -> IO FilePath
shortenHomePath p = do
  mh <- Env.lookupEnv "HOME"
  case mh of
    Nothing -> pure p
    Just h ->
      case stripPrefix (ensureTrailingSlash h) p of
        Just rest -> pure ("~/" <> rest)
        Nothing -> if p == h then pure "~" else pure p
  where
    ensureTrailingSlash s
      | null s = s
      | last s == '/' = s
      | otherwise = s <> "/"

gitBranchIn :: FilePath -> IO (Maybe String)
gitBranchIn dir = do
  isDir <- Dir.doesDirectoryExist dir
  if not isDir
    then pure Nothing
    else do
      r <-
        try (Proc.readCreateProcessWithExitCode (Proc.proc "git" ["-C", dir, "rev-parse", "--abbrev-ref", "HEAD"]) "") ::
          IO (Either IOException (Exit.ExitCode, String, String))
      case r of
        Left _ -> pure Nothing
        Right (ec, out, _err) ->
          if ec /= Exit.ExitSuccess
            then pure Nothing
            else do
              let b = trim (takeWhile (/= '\n') out)
              if null b || b == "HEAD" then pure Nothing else pure (Just b)
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

mkPromptArgsIO :: ShellState -> IO (FilePath, Maybe String, Int, Maybe Int)
mkPromptArgsIO st = do
  prettyCwd <- shortenHomePath (cwd st)
  branch <- gitBranchIn (cwd st)
  pure (prettyCwd, branch, lastExitCode st, lastDurationMs st)
