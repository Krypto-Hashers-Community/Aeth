{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Aeth.Exec
  ( runPipeline,
    runPipelineCapture,
  )
where

import Aeth.Parse (parsePipeline)
import Aeth.Structured
import Aeth.Types
import Control.Exception (IOException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (StateT, get, put)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.FilePath as FP
import System.IO (hClose, stderr)
import qualified System.Process as Proc

-- | Shared list of shell builtin commands
shellBuiltins :: [T.Text]
shellBuiltins =
  [ "cd",
    "exit",
    "export",
    "unset",
    "pwd",
    "history",
    "clear",
    "source",
    ".",
    "type",
    "which",
    "echo",
    "true",
    "false",
    "jobs"
  ]

exitCodeToInt :: Exit.ExitCode -> Int
exitCodeToInt ec =
  case ec of
    Exit.ExitSuccess -> 0
    Exit.ExitFailure n -> n

setLastExit :: (MonadIO m) => Exit.ExitCode -> StateT ShellState m ()
setLastExit ec = do
  st <- get
  put st {lastExitCode = exitCodeToInt ec}

-- | Type alias for alias map
type AliasMap = Map.Map T.Text T.Text

-- | Expand aliases in a segment
expandAlias :: AliasMap -> Segment -> Segment
expandAlias aliasMap seg =
  case Map.lookup (segName seg) aliasMap of
    Nothing -> seg
    Just expanded ->
      let tokens = T.words expanded
       in case tokens of
            [] -> seg
            (cmd : args) ->
              let isStructured = T.isPrefixOf "@" cmd
                  mode = if isStructured then Structured else RawString
                  name = if isStructured then T.drop 1 cmd else cmd
               in seg {segMode = mode, segName = name, segArgs = args ++ segArgs seg}

runPipeline :: (MonadIO m) => AliasMap -> Pipeline -> StateT ShellState m ()
runPipeline aliasMap (Pipeline segments) =
  let expandedSegments = map (expandAlias aliasMap) segments
   in case expandedSegments of
        [] -> pure ()
        [seg] -> runSingle seg
        _ -> runMulti expandedSegments

-- | Like 'runPipeline' but captures stdout/stderr into a returned Text.
-- Used by the interactive Vty UI so output stays visible.
runPipelineCapture :: Pipeline -> StateT ShellState IO T.Text
runPipelineCapture (Pipeline segments) =
  case segments of
    [] -> pure ""
    [seg] -> runSingleCapture seg
    _ -> runMultiCapture segments

runSingle :: (MonadIO m) => Segment -> StateT ShellState m ()
runSingle seg =
  case segMode seg of
    Structured -> runStructured seg
    RawString -> runRawSingle seg

runSingleCapture :: Segment -> StateT ShellState IO T.Text
runSingleCapture seg =
  case segMode seg of
    Structured -> do
      (v, ec) <- structuredValueFor seg
      setLastExit ec
      pure (renderStructured v)
    RawString -> runRawSingleCapture seg

-- | Check if a segment should be treated as structured, either explicitly (@cmd)
-- or implicitly (e.g. filter, select).
isStructuredSegment :: Segment -> Bool
isStructuredSegment s =
  segMode s == Structured || segName s `elem` ["filter", "select", "sort"]

runMulti :: (MonadIO m) => [Segment] -> StateT ShellState m ()
runMulti segs = do
  -- Minimal rule for now:
  -- - Structured pipelines can feed into raw pipelines by rendering structured output to stdin.
  -- - Fully-structured pipelines are not implemented yet.
  -- - Raw pipelines are executed via /bin/sh -c for correctness (quoting, redirects).
  let anyStructured = any isStructuredSegment segs
  if not anyStructured
    then do
      let cmdline = renderRawSegments segs
      runViaSh cmdline
    else case segs of
      (s0 : _)
        | segMode s0 == Structured || segName s0 `elem` ["filter", "select", "sort"] -> do
            let (structuredHead, rawTail) = splitStructuredHead segs
            (v, ec) <- runStructuredChain structuredHead
            setLastExit ec
            let stdinText = renderStructured v
            let cmdline = renderRawSegments rawTail
            if T.null (T.strip cmdline)
              then liftIO (TIO.putStrLn stdinText)
              else runViaShWithStdin stdinText cmdline
      _ ->
        liftIO $
          TIO.putStrLn
            "Structured pipelines require the first stage to be structured (start with @cmd, e.g. @ls)"

renderRawSegments :: [Segment] -> T.Text
renderRawSegments segs =
  T.intercalate
    " | "
    [ T.unwords (segName s : segArgs s)
      | s <- segs
    ]

runMultiCapture :: [Segment] -> StateT ShellState IO T.Text
runMultiCapture segs = do
  let anyStructured = any isStructuredSegment segs
  if not anyStructured
    then do
      let cmdline = renderRawSegments segs
      runViaShCapture cmdline
    else do
      case segs of
        (s0 : _)
          | segMode s0 == Structured || segName s0 `elem` ["filter", "select", "sort"] -> do
              let (structuredHead, rawTail) = splitStructuredHead segs
              (v, ec) <- runStructuredChain structuredHead
              setLastExit ec
              let stdinText = renderStructured v
              let cmdline = renderRawSegments rawTail
              if T.null (T.strip cmdline)
                then pure stdinText
                else runViaShWithStdinCapture stdinText cmdline
        _ ->
          pure "Structured pipelines require the first stage to be structured (start with @cmd, e.g. @ls)"

splitStructuredHead :: [Segment] -> ([Segment], [Segment])
splitStructuredHead = go True []
  where
    go _ acc [] = (reverse acc, [])
    go inStructured acc (s : rest)
      | inStructured && isStructuredTransformLike s = go True (asStructuredTransform s : acc) rest
      | otherwise = (reverse acc, s : rest)

isStructuredTransformLike :: Segment -> Bool
isStructuredTransformLike s =
  segMode s == Structured || segName s `elem` ["filter", "select", "sort"]

asStructuredTransform :: Segment -> Segment
asStructuredTransform s =
  if segMode s == Structured
    then s
    else s {segMode = Structured}

runStructuredChain :: (MonadIO m) => [Segment] -> StateT ShellState m (StructuredValue, Exit.ExitCode)
runStructuredChain segs =
  case segs of
    [] -> pure (SText "", Exit.ExitFailure 2)
    (s0 : rest) -> do
      (v0, ec0) <- structuredValueFor s0
      foldl step (pure (v0, ec0)) rest
  where
    step accM s = do
      (vPrev, _ecPrev) <- accM
      structuredApply s vPrev

structuredApply :: (MonadIO m) => Segment -> StructuredValue -> StateT ShellState m (StructuredValue, Exit.ExitCode)
structuredApply seg input =
  case segName seg of
    "filter" -> do
      let expr = T.unwords (segArgs seg)
      case filterStructured expr input of
        Left e -> pure (SText ("Aeth: " <> e), Exit.ExitFailure 2)
        Right out -> pure (out, Exit.ExitSuccess)
    "sort" -> do
      let col = T.unwords (segArgs seg)
      case sortStructured col input of
        Left e -> pure (SText ("Aeth: " <> e), Exit.ExitFailure 2)
        Right out -> pure (out, Exit.ExitSuccess)
    "select" -> do
      case selectStructured (segArgs seg) input of
        Left e -> pure (SText ("Aeth: " <> e), Exit.ExitFailure 2)
        Right out -> pure (out, Exit.ExitSuccess)
    _ -> do
      -- Fallback: run as external command that consumes rendered input and captures stdout.
      st <- get
      let envMap = effectiveEnvMap st
      let name' = expandTextEnv envMap (segName seg)
      let args' = map (expandTextEnv envMap) (segArgs seg)
      liftIO $ externalToStructuredStdin (cwd st) envMap name' args' (renderStructured input)

externalToStructuredStdin :: FilePath -> Map.Map String String -> T.Text -> [T.Text] -> T.Text -> IO (StructuredValue, Exit.ExitCode)
externalToStructuredStdin workingDir envMap name args stdinText = do
  expandedArgs <- expandGlobs envMap workingDir args
  let envMerged = Map.toList envMap
  let cp =
        (Proc.proc (T.unpack name) (map T.unpack expandedArgs))
          { Proc.cwd = Just workingDir,
            Proc.env = Just envMerged
          }
  result <- try (Proc.readCreateProcessWithExitCode cp (T.unpack stdinText)) :: IO (Either IOException (Exit.ExitCode, String, String))
  case result of
    Left e -> pure (SText ("Aeth: " <> renderExecException name e), Exit.ExitFailure 127)
    Right (ec, out, err) ->
      case ec of
        Exit.ExitSuccess -> pure (SText (T.pack out), ec)
        _ ->
          pure (SText (T.pack out <> (if null err then "" else "\n" <> T.pack err) <> "\n" <> "exit: " <> T.pack (show ec)), ec)

-- | Helper for cd: resolve target and change directory
doCd :: (MonadIO m) => ShellState -> Map.Map String String -> Maybe String -> StateT ShellState m ()
doCd st envMap target = do
  result <- liftIO (try (resolveCdTarget envMap target >>= Dir.setCurrentDirectory) :: IO (Either IOException ()))
  case result of
    Left e -> do
      liftIO (hPutShellError ("cd: " <> T.pack (show e)))
      put st {lastExitCode = 1}
    Right () -> do
      newDir <- liftIO Dir.getCurrentDirectory
      let oldDir = cwd st
      put
        st
          { cwd = newDir,
            envOverrides =
              Map.insert
                "PWD"
                newDir
                (Map.insert "OLDPWD" oldDir (envOverrides st)),
            lastExitCode = 0
          }

-- | Helper for cd in capture context: resolve target and change directory, return output
doCdCapture :: ShellState -> Map.Map String String -> Maybe String -> StateT ShellState IO T.Text
doCdCapture st envMap target = do
  result <- liftIO (try (resolveCdTarget envMap target >>= Dir.setCurrentDirectory) :: IO (Either IOException ()))
  case result of
    Left e -> do
      put st {lastExitCode = 1}
      pure ("cd: " <> T.pack (show e))
    Right () -> do
      newDir <- liftIO Dir.getCurrentDirectory
      let oldDir = cwd st
      put
        st
          { cwd = newDir,
            envOverrides =
              Map.insert
                "PWD"
                newDir
                (Map.insert "OLDPWD" oldDir (envOverrides st)),
            lastExitCode = 0
          }
      pure ""

runRawSingle :: (MonadIO m) => Segment -> StateT ShellState m ()
runRawSingle seg = do
  let name = segName seg
  let args = segArgs seg
  if name == ""
    then pure ()
    else case name of
      "exit" -> liftIO (Exit.exitWith Exit.ExitSuccess)
      "export" -> runExport args
      "unset" -> runUnset args
      "pwd" -> runPwd
      "history" -> runHistory
      "clear" -> liftIO $ do
        putStr "\ESC[H\ESC[2J\ESC[3J" -- ANSI clear screen + scrollback
      "source" -> runSource args
      "." -> runSource args -- POSIX alias for source
      "type" -> runType args
      "which" -> runWhich args
      "echo" -> runEcho args
      "true" -> setLastExit Exit.ExitSuccess
      "false" -> setLastExit (Exit.ExitFailure 1)
      "jobs" -> runJobs
      "cd" -> do
        st <- get
        let envMap = effectiveEnvMap st
        -- Handle "cd -" specially: error if OLDPWD not set
        case args of
          ("-" : _) ->
            case Map.lookup "OLDPWD" envMap of
              Nothing -> do
                liftIO (hPutShellError "cd: OLDPWD not set")
                put st {lastExitCode = 1}
              Just oldpwd -> doCd st envMap (Just oldpwd)
          [] -> doCd st envMap Nothing
          (p : _) -> doCd st envMap (Just (T.unpack (expandTextEnv envMap p)))
      _ ->
        if shouldUseShSegment seg
          then do
            -- Delegate complex shell syntax to /bin/sh for compatibility.
            -- Note: this does not persist any env changes back into Aeth state.
            let cmdline = renderRawSegment seg
            runViaSh cmdline
          else runExternal name args

runRawSingleCapture :: Segment -> StateT ShellState IO T.Text
runRawSingleCapture seg = do
  let name = segName seg
  let args = segArgs seg
  if name == ""
    then pure ""
    else case name of
      "exit" -> liftIO (Exit.exitWith Exit.ExitSuccess)
      "export" -> do
        runExport args
        pure ""
      "cd" -> do
        st <- get
        let envMap = effectiveEnvMap st
        -- Handle "cd -" specially: error if OLDPWD not set
        case args of
          ("-" : _) ->
            case Map.lookup "OLDPWD" envMap of
              Nothing -> do
                put st {lastExitCode = 1}
                pure "cd: OLDPWD not set"
              Just oldpwd -> doCdCapture st envMap (Just oldpwd)
          [] -> doCdCapture st envMap Nothing
          (p : _) -> doCdCapture st envMap (Just (T.unpack (expandTextEnv envMap p)))
      _ ->
        if shouldUseShSegment seg
          then runViaShCapture (renderRawSegment seg)
          else runExternalCapture name args

renderRawSegment :: Segment -> T.Text
renderRawSegment s = T.unwords (segName s : segArgs s)

shouldUseShSegment :: Segment -> Bool
shouldUseShSegment s =
  let cmd = renderRawSegment s
   in T.any (`elem` (";&><()\\`" :: String)) cmd
        || T.isInfixOf "&&" cmd
        || T.isInfixOf "||" cmd
        || T.isInfixOf "|&" cmd

runExternal :: (MonadIO m) => T.Text -> [T.Text] -> StateT ShellState m ()
runExternal name args = do
  st <- get
  let envMap = effectiveEnvMap st
  let name' = expandTextEnv envMap name
  let args' = map (expandTextEnv envMap) args
  expandedArgs <- liftIO (expandGlobs envMap (cwd st) args')
  let envMerged = Map.toList envMap
  let cp =
        (Proc.proc (T.unpack name') (map T.unpack expandedArgs))
          { Proc.cwd = Just (cwd st),
            Proc.env = Just envMerged
          }
  result <- liftIO (try (Proc.withCreateProcess cp (\_ _ _ ph -> Proc.waitForProcess ph)) :: IO (Either IOException Exit.ExitCode))
  case result of
    Left e -> do
      liftIO (hPutShellError (renderExecException name e))
      put st {lastExitCode = 127}
    Right ec -> do
      setLastExit ec
      when (ec /= Exit.ExitSuccess) $ liftIO (hPutShellError ("exit: " <> T.pack (show ec)))

runExternalCapture :: T.Text -> [T.Text] -> StateT ShellState IO T.Text
runExternalCapture name args = do
  st <- get
  let envMap = effectiveEnvMap st
  let name' = expandTextEnv envMap name
  let args' = map (expandTextEnv envMap) args
  expandedArgs <- liftIO (expandGlobs envMap (cwd st) args')
  let envMerged = Map.toList envMap
  let cp =
        (Proc.proc (T.unpack name') (map T.unpack expandedArgs))
          { Proc.cwd = Just (cwd st),
            Proc.env = Just envMerged
          }
  result <- liftIO (try (Proc.readCreateProcessWithExitCode cp "") :: IO (Either IOException (Exit.ExitCode, String, String)))
  case result of
    Left e -> do
      put st {lastExitCode = 127}
      pure ("Aeth: " <> renderExecException name e)
    Right (ec, out, err) -> do
      setLastExit ec
      let outT = T.pack out
          errT = T.pack err
          extra =
            if ec == Exit.ExitSuccess
              then ""
              else "\n" <> "Aeth: exit: " <> T.pack (show ec)
          merged =
            T.stripEnd (outT <> (if T.null errT then "" else if T.null outT then errT else "\n" <> errT) <> extra)
      pure merged

runViaSh :: (MonadIO m) => T.Text -> StateT ShellState m ()
runViaSh cmdline = do
  st <- get
  let envMap = effectiveEnvMap st
  let envMerged = Map.toList envMap
  let cp =
        (Proc.proc "/bin/sh" ["-c", T.unpack cmdline])
          { Proc.cwd = Just (cwd st),
            Proc.env = Just envMerged
          }
  ec <- liftIO (Proc.withCreateProcess cp (\_ _ _ ph -> Proc.waitForProcess ph))
  setLastExit ec

runViaShCapture :: T.Text -> StateT ShellState IO T.Text
runViaShCapture cmdline = do
  st <- get
  let envMap = effectiveEnvMap st
  let envMerged = Map.toList envMap
  let cp =
        (Proc.proc "/bin/sh" ["-c", T.unpack cmdline])
          { Proc.cwd = Just (cwd st),
            Proc.env = Just envMerged
          }
  result <- liftIO (try (Proc.readCreateProcessWithExitCode cp "") :: IO (Either IOException (Exit.ExitCode, String, String)))
  case result of
    Left e -> do
      put st {lastExitCode = 127}
      pure ("Aeth: " <> T.pack (show e))
    Right (ec, out, err) -> do
      setLastExit ec
      let outT = T.pack out
          errT = T.pack err
          extra =
            if ec == Exit.ExitSuccess
              then ""
              else "\n" <> "Aeth: exit: " <> T.pack (show ec)
      pure (T.stripEnd (outT <> (if T.null errT then "" else if T.null outT then errT else "\n" <> errT) <> extra))

runViaShWithStdin :: (MonadIO m) => T.Text -> T.Text -> StateT ShellState m ()
runViaShWithStdin stdinText cmdline = do
  st <- get
  let envMap = effectiveEnvMap st
  let envMerged = Map.toList envMap
  let cp =
        (Proc.proc "/bin/sh" ["-c", T.unpack cmdline])
          { Proc.cwd = Just (cwd st),
            Proc.env = Just envMerged,
            Proc.std_in = Proc.CreatePipe
          }
  ec <- liftIO $ Proc.withCreateProcess cp $ \mIn _ _ ph -> do
    case mIn of
      Nothing -> pure ()
      Just hIn -> do
        TIO.hPutStr hIn stdinText
        hClose hIn
    Proc.waitForProcess ph
  setLastExit ec

runViaShWithStdinCapture :: T.Text -> T.Text -> StateT ShellState IO T.Text
runViaShWithStdinCapture stdinText cmdline = do
  st <- get
  let envMap = effectiveEnvMap st
  let envMerged = Map.toList envMap
  let cp =
        (Proc.proc "/bin/sh" ["-c", T.unpack cmdline])
          { Proc.cwd = Just (cwd st),
            Proc.env = Just envMerged
          }
  result <- liftIO (try (Proc.readCreateProcessWithExitCode cp (T.unpack stdinText)) :: IO (Either IOException (Exit.ExitCode, String, String)))
  case result of
    Left e -> do
      put st {lastExitCode = 127}
      pure ("Aeth: " <> T.pack (show e))
    Right (ec, outS, errS) -> do
      setLastExit ec
      let outT = T.pack outS
          errT = T.pack errS
          extra =
            if ec == Exit.ExitSuccess
              then ""
              else "\n" <> "Aeth: exit: " <> T.pack (show ec)
      pure (T.stripEnd (outT <> (if T.null errT then "" else if T.null outT then errT else "\n" <> errT) <> extra))

runStructured :: (MonadIO m) => Segment -> StateT ShellState m ()
runStructured seg = do
  (v, ec) <- structuredValueFor seg
  setLastExit ec
  liftIO (TIO.putStrLn (renderStructured v))

structuredValueFor :: (MonadIO m) => Segment -> StateT ShellState m (StructuredValue, Exit.ExitCode)
structuredValueFor seg = do
  st <- get
  let envMap = effectiveEnvMap st
  let args' = map (expandTextEnv envMap) (segArgs seg)
  let name = segName seg
  liftIO $
    case name of
      "ls" -> do
        let (opts, pathArgs) = parseLsArgs args'
        let targetDir = case pathArgs of
              (p : _) -> T.unpack p
              _ -> cwd st
        v <- lsStructured opts targetDir
        pure (v, Exit.ExitSuccess)
      "pwd" -> do
        v <- pwdStructured (cwd st)
        pure (v, Exit.ExitSuccess)
      "ps" -> do
        v <- psStructured
        pure (v, Exit.ExitSuccess)
      "df" -> do
        v <- dfStructured
        pure (v, Exit.ExitSuccess)
      "env" -> do
        v <- envStructured
        pure (v, Exit.ExitSuccess)
      "find" -> do
        let (basePath, patterns) = case args' of
              [] -> (cwd st, [])
              (p : rest) -> (T.unpack p, rest)
        v <- findStructured basePath patterns
        pure (v, Exit.ExitSuccess)
      _ -> do
        -- For now, treat unknown @commands as "capture external stdout".
        -- This enables: @cmd args | grep ...
        externalToStructured (cwd st) envMap (expandTextEnv envMap name) args'

externalToStructured :: FilePath -> Map.Map String String -> T.Text -> [T.Text] -> IO (StructuredValue, Exit.ExitCode)
externalToStructured workingDir envMap name args = do
  expandedArgs <- expandGlobs envMap workingDir args
  let envMerged = Map.toList envMap
  let cp =
        (Proc.proc (T.unpack name) (map T.unpack expandedArgs))
          { Proc.cwd = Just workingDir,
            Proc.env = Just envMerged
          }
  -- readCreateProcessWithExitCode captures stdout/stderr (good enough for now).
  result <- try (Proc.readCreateProcessWithExitCode cp "") :: IO (Either IOException (Exit.ExitCode, String, String))
  case result of
    Left e -> pure (SText ("Aeth: " <> renderExecException name e), Exit.ExitFailure 127)
    Right (ec, out, err) ->
      case ec of
        Exit.ExitSuccess -> pure (SText (T.pack out), ec)
        _ ->
          pure (SText (T.pack out <> (if null err then "" else "\n" <> T.pack err) <> "\n" <> "exit: " <> T.pack (show ec)), ec)

hPutShellError :: T.Text -> IO ()
hPutShellError msg = TIO.hPutStrLn stderr ("Aeth: " <> msg)

renderExecException :: T.Text -> IOException -> T.Text
renderExecException cmd e =
  let msg = T.pack (show e)
   in if isCommandNotFound msg
        then "command not found: " <> cmd
        else "exec: " <> msg

isCommandNotFound :: T.Text -> Bool
isCommandNotFound msg =
  -- Heuristic: different platforms/RTS versions format this differently.
  T.isInfixOf "does not exist" msg || T.isInfixOf "No such file or directory" msg

expandGlobs :: Map.Map String String -> FilePath -> [T.Text] -> IO [T.Text]
expandGlobs envMap workingDir args = do
  let args1 = map (expandArgShorthands envMap) args
  fmap concat (mapM (expandOne workingDir) args1)

expandArgShorthands :: Map.Map String String -> T.Text -> T.Text
expandArgShorthands envMap t =
  let p1 = expandTildeText envMap t
   in expandTextEnv envMap p1

expandOne :: FilePath -> T.Text -> IO [T.Text]
expandOne workingDir arg
  | not (hasGlobChars arg) = pure [arg]
  | otherwise = do
      let raw = T.unpack arg
      let (dirPart, pattern) = splitDirPattern raw
      if any isGlobChar dirPart
        then pure [arg]
        else do
          let listDirPath =
                if null dirPart || dirPart == "."
                  then workingDir
                  else
                    if FP.isAbsolute dirPart
                      then dirPart
                      else workingDir FP.</> dirPart
          exists <- Dir.doesDirectoryExist listDirPath
          if not exists
            then pure [arg]
            else do
              entries <- Dir.listDirectory listDirPath
              let isDot = case pattern of
                    ('.' : _) -> True
                    _ -> False
              let candidates =
                    filter
                      (\n -> (isDot || not (List.isPrefixOf "." n)) && matchGlob pattern n)
                      entries
              let sorted = List.sort candidates
              if null sorted
                then pure [arg]
                else
                  pure
                    (map (T.pack . (if null dirPart || dirPart == "." then id else (dirPart FP.</>))) sorted)

splitDirPattern :: FilePath -> (FilePath, String)
splitDirPattern p =
  let dirPart = FP.takeDirectory p
      basePart = FP.takeFileName p
   in if dirPart == "." then ("", basePart) else (dirPart, basePart)

hasGlobChars :: T.Text -> Bool
hasGlobChars = T.any isGlobChar

isGlobChar :: Char -> Bool
isGlobChar c = c == '*' || c == '?' || c == '['

matchGlob :: String -> String -> Bool
matchGlob pat str = go pat str
  where
    go [] [] = True
    go [] _ = False
    go ('*' : ps) s = any (go ps) (suffixes s)
    go ('?' : ps) (_ : ss) = go ps ss
    go ('?' : _) [] = False
    go ('[' : ps) s0 =
      case s0 of
        [] -> False
        (c : cs) ->
          case parseCharClass ps of
            Nothing -> go ('[' : ps) s0
            Just (neg, allowed, rest) ->
              let ok = (c `elem` allowed) /= neg
               in ok && go rest cs
    go (p : ps) (c : cs) = p == c && go ps cs
    go _ [] = False

    suffixes :: String -> [String]
    suffixes s =
      s : case s of
        [] -> []
        (_ : xs) -> suffixes xs

parseCharClass :: String -> Maybe (Bool, [Char], String)
parseCharClass s =
  let (neg, s1) =
        case s of
          ('!' : xs) -> (True, xs)
          _ -> (False, s)
      (body, rest0) = break (== ']') s1
   in case rest0 of
        [] -> Nothing
        (_ : rest) -> Just (neg, expandRanges body, rest)

expandRanges :: String -> [Char]
expandRanges = go
  where
    go [] = []
    go (a : '-' : b : xs) =
      if a <= b
        then [a .. b] ++ go xs
        else [b .. a] ++ go xs
    go (x : xs) = x : go xs

resolveCdTarget :: Map.Map String String -> Maybe FilePath -> IO FilePath
resolveCdTarget envMap mTarget =
  case mTarget of
    Just p -> do
      let p1 = T.unpack (expandTildeText envMap (T.pack p))
      let p2 = T.unpack (expandTextEnv envMap (T.pack p1))
      Dir.makeAbsolute p2
    Nothing -> do
      Dir.makeAbsolute (Map.findWithDefault "." "HOME" envMap)

-- | Merge base env with overrides (overrides win), excluding deleted vars.
effectiveEnvMap :: ShellState -> Map.Map String String
effectiveEnvMap st =
  let merged = Map.union (envOverrides st) (baseEnv st)
   in foldr Map.delete merged (Set.toList (deletedEnv st))

-- | Tilde expansion for arguments (only leading ~ forms).
expandTildeText :: Map.Map String String -> T.Text -> T.Text
expandTildeText envMap t =
  case T.unpack t of
    "~" -> T.pack (Map.findWithDefault "~" "HOME" envMap)
    '~' : '/' : rest ->
      case Map.lookup "HOME" envMap of
        Nothing -> t
        Just h -> T.pack (h FP.</> rest)
    _ -> t

-- | Expand $VAR and ${VAR} anywhere in the text.
--
-- This is intentionally minimal (no quoting/escape rules yet).
expandTextEnv :: Map.Map String String -> T.Text -> T.Text
expandTextEnv envMap = go
  where
    go s =
      case T.breakOn "$" s of
        (before, rest)
          | T.null rest -> before
          | otherwise ->
              let afterDollar = T.drop 1 rest
               in before <> expandVar afterDollar

    expandVar s
      | T.null s = "$"
      | T.isPrefixOf "{" s =
          case T.breakOn "}" (T.drop 1 s) of
            (var, tail0)
              | T.null tail0 -> "${" <> var
              | otherwise ->
                  let value = Map.findWithDefault ("${" <> T.unpack var <> "}") (T.unpack var) envMap
                      rest = T.drop 1 tail0
                   in T.pack value <> go rest
      | otherwise =
          let (var, rest) = T.span isVarChar s
           in if T.null var
                then "$" <> go s
                else
                  let value = Map.findWithDefault ("$" <> T.unpack var) (T.unpack var) envMap
                   in T.pack value <> go rest

    isVarChar c =
      (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c == '_'

runExport :: (MonadIO m) => [T.Text] -> StateT ShellState m ()
runExport args = do
  st <- get
  let envMap = effectiveEnvMap st
  case args of
    [] -> liftIO $ mapM_ putKV (Map.toList envMap)
    _ -> do
      newOverrides <- foldl (step envMap) (pure (envOverrides st)) args
      put st {envOverrides = newOverrides, lastExitCode = 0}
  where
    putKV (k, v) = TIO.hPutStrLn stderr (T.pack (k <> "=" <> v))

    step :: (MonadIO m) => Map.Map String String -> m (Map.Map String String) -> T.Text -> m (Map.Map String String)
    step envMap accM a = do
      acc <- accM
      let t = expandTextEnv envMap a
      case T.breakOn "=" t of
        (k, vEq)
          | T.null vEq ->
              -- export KEY: keep existing value if present, else set empty
              let key = T.unpack (T.strip k)
                  val = Map.findWithDefault "" key envMap
               in pure (Map.insert key val acc)
          | otherwise ->
              let key = T.unpack (T.strip k)
                  val = T.unpack (T.drop 1 vEq)
               in pure (Map.insert key val acc)

-- | unset - remove environment variables (adds to tombstone set so inherited vars are also unset)
runUnset :: (MonadIO m) => [T.Text] -> StateT ShellState m ()
runUnset args = do
  st <- get
  let keys = map T.unpack args
      newOverrides = foldr Map.delete (envOverrides st) keys
      newDeleted = foldr Set.insert (deletedEnv st) keys
  put st {envOverrides = newOverrides, deletedEnv = newDeleted, lastExitCode = 0}

-- | pwd - print working directory
runPwd :: (MonadIO m) => StateT ShellState m ()
runPwd = do
  st <- get
  liftIO $ putStrLn (cwd st)
  setLastExit Exit.ExitSuccess

-- | history - show command history
runHistory :: (MonadIO m) => StateT ShellState m ()
runHistory = do
  st <- get
  let hist = history st
  liftIO $ mapM_ (\(i, cmd) -> putStrLn $ show i ++ "  " ++ cmd) (zip ([1 ..] :: [Int]) hist)
  setLastExit Exit.ExitSuccess

-- | source - execute commands from a file
-- Executes each line in the current shell context so exports/cd persist.
-- Note: Aliases are not expanded in sourced files.
runSource :: (MonadIO m) => [T.Text] -> StateT ShellState m ()
runSource args = do
  case args of
    [] -> do
      liftIO $ hPutShellError "source: filename argument required"
      setLastExit (Exit.ExitFailure 1)
    (path : _) -> do
      st <- get
      let envMap = effectiveEnvMap st
      let expandedPath = T.unpack (expandTextEnv envMap path)
      exists <- liftIO $ Dir.doesFileExist expandedPath
      if not exists
        then do
          liftIO $ hPutShellError ("source: " <> T.pack expandedPath <> ": No such file")
          setLastExit (Exit.ExitFailure 1)
        else do
          contentResult <- liftIO $ try (TIO.readFile expandedPath)
          case contentResult of
            Left (e :: IOException) -> do
              liftIO $ hPutShellError ("source: " <> T.pack (show e))
              setLastExit (Exit.ExitFailure 1)
            Right content -> do
              let lns = filter (not . T.null) (map stripComments (T.lines content))
              -- Execute each line in the current shell context
              mapM_ executeSourceLine lns
  where
    stripComments t = T.strip (T.takeWhile (/= '#') t)

    executeSourceLine lineText = do
      case parsePipeline lineText of
        Left err -> do
          when (err /= "empty") $ do
            liftIO $ hPutShellError ("source: parse error: " <> T.pack err)
            setLastExit (Exit.ExitFailure 2)
        Right pipeline -> runPipeline Map.empty pipeline

-- | type - describe a command
runType :: (MonadIO m) => [T.Text] -> StateT ShellState m ()
runType args = do
  st <- get
  let envMap = effectiveEnvMap st
  -- Use the shared shellBuiltins constant
  results <- mapM (checkCmd envMap) args
  -- Set exit status based on whether all commands were found
  if and results
    then setLastExit Exit.ExitSuccess
    else setLastExit (Exit.ExitFailure 1)
  where
    checkCmd envMap cmd = do
      if cmd `elem` shellBuiltins
        then do
          liftIO $ TIO.putStrLn (cmd <> " is a shell builtin")
          pure True
        else do
          mPath <- liftIO $ findExecutable envMap (T.unpack cmd)
          case mPath of
            Just p -> do
              liftIO $ TIO.putStrLn (cmd <> " is " <> T.pack p)
              pure True
            Nothing -> do
              liftIO $ TIO.putStrLn (cmd <> " not found")
              pure False

-- | which - find executable
runWhich :: (MonadIO m) => [T.Text] -> StateT ShellState m ()
runWhich args = do
  st <- get
  let envMap = effectiveEnvMap st
  allFound <- mapM (checkCmd envMap) args
  setLastExit $ if and allFound then Exit.ExitSuccess else Exit.ExitFailure 1
  where
    checkCmd envMap cmd = do
      mPath <- liftIO $ findExecutable envMap (T.unpack cmd)
      case mPath of
        Just p -> do
          liftIO $ putStrLn p
          pure True
        Nothing -> pure False

-- | echo - print arguments
runEcho :: (MonadIO m) => [T.Text] -> StateT ShellState m ()
runEcho args = do
  st <- get
  let envMap = effectiveEnvMap st
  let expanded = map (expandTextEnv envMap) args
  liftIO $ TIO.putStrLn (T.unwords expanded)
  setLastExit Exit.ExitSuccess

-- | jobs - list background jobs
runJobs :: (MonadIO m) => StateT ShellState m ()
runJobs = do
  st <- get
  let jobs = backgroundJobs st
  if null jobs
    then setLastExit Exit.ExitSuccess
    else do
      liftIO $ mapM_ printJob jobs
      setLastExit Exit.ExitSuccess
  where
    printJob job =
      TIO.putStrLn $
        "["
          <> T.pack (show (jobId job))
          <> "] "
          <> T.pack (show (jobStatus job))
          <> " "
          <> jobCommand job

-- | Find executable in PATH, or test directly if name contains '/'
findExecutable :: Map.Map String String -> String -> IO (Maybe FilePath)
findExecutable envMap name
  -- If name contains '/', test it directly instead of searching PATH
  | elem '/' name = do
      exists <- Dir.doesFileExist name
      isExe <- if exists then Dir.executable <$> Dir.getPermissions name else pure False
      pure (if exists && isExe then Just name else Nothing)
  | otherwise = do
      let pathEnv = Map.findWithDefault "" "PATH" envMap
          pathDirs = FP.splitSearchPath pathEnv
      findFirst pathDirs
  where
    findFirst [] = pure Nothing
    findFirst (d : ds) = do
      let fullPath = d FP.</> name
      exists <- Dir.doesFileExist fullPath
      isExe <- if exists then Dir.executable <$> Dir.getPermissions fullPath else pure False
      if exists && isExe
        then pure (Just fullPath)
        else findFirst ds
