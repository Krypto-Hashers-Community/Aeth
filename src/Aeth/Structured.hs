{-# LANGUAGE OverloadedStrings #-}

module Aeth.Structured
  ( StructuredValue (..),
    renderStructured,
    lsStructured,
    pwdStructured,
    filterStructured,
    psStructured,
    dfStructured,
    envStructured,
    findStructured,
    sortStructured,
    selectStructured,
    LsOptions (..),
    defaultLsOptions,
    parseLsArgs,
  )
where

import Control.Exception (IOException, try)
import Data.Char (isDigit, toLower)
import Data.Int (Int64)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified System.Directory as Dir
import qualified System.Environment as Env
import qualified System.FilePath as FP
import qualified System.Posix.Files as Posix
import System.Posix.Types (DeviceID, FileID, FileMode)
import qualified System.Process as Proc
import Text.Printf (printf)

-- Keep it simple: a small structured algebra we can extend.

data StructuredValue
  = SText T.Text
  | STable [T.Text] [[T.Text]]
  deriving (Eq, Show)

-- | Options for @ls command
data LsOptions = LsOptions
  { lsShowAll :: Bool, -- -a: show hidden files
    lsLongFormat :: Bool, -- -l: long format with details
    lsHumanReadable :: Bool, -- -h: human readable sizes
    lsSortBySize :: Bool, -- -S: sort by size
    lsSortByTime :: Bool, -- -t: sort by modification time
    lsReverse :: Bool -- -r: reverse sort order
  }
  deriving (Eq, Show)

defaultLsOptions :: LsOptions
defaultLsOptions =
  LsOptions
    { lsShowAll = False,
      lsLongFormat = False,
      lsHumanReadable = True,
      lsSortBySize = False,
      lsSortByTime = False,
      lsReverse = False
    }

-- | Parse ls command line arguments
parseLsArgs :: [T.Text] -> (LsOptions, [T.Text])
parseLsArgs args = (opts, paths)
  where
    (flags, paths) = List.partition (T.isPrefixOf "-") args
    opts = foldr applyFlag defaultLsOptions flags

    -- Apply all flag characters cumulatively so "-la" enables both -l and -a
    applyFlag flag o =
      let chars = T.unpack (T.drop 1 flag) -- drop leading '-'
       in foldl applyOneFlag o chars

    applyOneFlag o c = case c of
      'a' -> o {lsShowAll = True}
      'l' -> o {lsLongFormat = True}
      'h' -> o {lsHumanReadable = True}
      'S' -> o {lsSortBySize = True}
      't' -> o {lsSortByTime = True}
      'r' -> o {lsReverse = True}
      _ -> o -- ignore unknown flags

-- | Filter a structured table with a tiny expression language.
--
-- Supported forms (spaces required around operator):
--   - .size > 1MB
--   - .size >= 1024
--   - .name == foo
--   - .kind != dir
--   - .name contains kde
--
-- Braces are allowed and ignored, e.g. "{ .size > 1MB }".
filterStructured :: T.Text -> StructuredValue -> Either T.Text StructuredValue
filterStructured rawExpr v =
  case v of
    SText _ -> Left "filter: expected table input"
    STable headers rows -> do
      (field, op, lit) <- parsePredicate rawExpr
      -- Handle common aliases like permission -> permissions
      let actualField = case field of
            "permission" -> "permissions"
            f -> f
      idx <- maybe (Left ("filter: unknown field: " <> field)) Right (List.elemIndex actualField headers)
      let keepRow r =
            let cell = if idx < length r then r !! idx else ""
             in evalPredicate actualField op lit cell
      pure (STable headers (filter keepRow rows))

data Op = OpEq | OpNe | OpGt | OpGe | OpLt | OpLe | OpContains

parsePredicate :: T.Text -> Either T.Text (T.Text, Op, T.Text)
parsePredicate expr0 =
  let expr1 = stripBraces (T.strip expr0)
      toks = T.words expr1
   in case toks of
        (f : o : rest) | not (null rest) -> do
          op <- parseOp o
          pure (normalizeField f, op, T.unwords rest)
        _ -> Left "filter: expected: <field> <op> <value>"
  where
    stripBraces t =
      let t1 = T.dropWhile (== '{') (T.dropWhileEnd (== '}') t)
       in T.strip t1

    normalizeField f =
      let f1 = T.strip f
       in if T.isPrefixOf "." f1 then T.drop 1 f1 else f1

    parseOp t =
      case T.strip t of
        "==" -> Right OpEq
        "!=" -> Right OpNe
        ">" -> Right OpGt
        ">=" -> Right OpGe
        "<" -> Right OpLt
        "<=" -> Right OpLe
        "contains" -> Right OpContains
        _ -> Left ("filter: unknown operator: " <> t)

-- | Strip ANSI escape codes
stripAnsi :: T.Text -> T.Text
stripAnsi t =
  let (before, match) = T.breakOn "\ESC[" t
   in if T.null match
        then before
        else before <> stripAnsi (T.drop 1 (T.dropWhile (/= 'm') (T.drop 2 match)))

evalPredicate :: T.Text -> Op -> T.Text -> T.Text -> Bool
evalPredicate field op lit cellRaw =
  let cell = stripAnsi cellRaw
   in case field of
        "size" ->
          case (parseSizeBytes cell, parseSizeBytes lit) of
            (Just a, Just b) -> cmpNum op a b
            _ -> False
        _ ->
          let a = cell
              b = stripQuotes (T.strip lit)
           in case op of
                OpEq -> a == b
                OpNe -> a /= b
                OpContains -> T.toCaseFold b `T.isInfixOf` T.toCaseFold a
                OpGt -> a > b
                OpGe -> a >= b
                OpLt -> a < b
                OpLe -> a <= b
  where
    stripQuotes t =
      case (T.uncons t, T.unsnoc t) of
        (Just ('\"', _), Just (_, '\"')) -> T.drop 1 (T.dropEnd 1 t)
        (Just ('\'', _), Just (_, '\'')) -> T.drop 1 (T.dropEnd 1 t)
        _ -> t

cmpNum :: Op -> Int64 -> Int64 -> Bool
cmpNum op a b =
  case op of
    OpEq -> a == b
    OpNe -> a /= b
    OpGt -> a > b
    OpGe -> a >= b
    OpLt -> a < b
    OpLe -> a <= b
    OpContains -> False

parseSizeBytes :: T.Text -> Maybe Int64
parseSizeBytes t0 =
  let t = stripAnsi (T.strip t0)
      (numTxt, unitTxt) = T.span (\c -> isDigit c || c == '.') t
   in case reads (T.unpack numTxt) :: [(Double, String)] of
        [(n, "")] ->
          let unit = map toLower (T.unpack (T.strip unitTxt))
           in Just (floor (n * fromIntegral (unitMultiplier unit)))
        _ -> Nothing

unitMultiplier :: String -> Int64
unitMultiplier u =
  case u of
    "" -> 1
    "b" -> 1
    "k" -> 1000
    "kb" -> 1000
    "m" -> 1000 * 1000
    "mb" -> 1000 * 1000
    "g" -> 1000 * 1000 * 1000
    "gb" -> 1000 * 1000 * 1000
    "kib" -> 1024
    "mib" -> 1024 * 1024
    "gib" -> 1024 * 1024 * 1024
    _ -> 1

renderStructured :: StructuredValue -> T.Text
renderStructured v =
  case v of
    SText t -> t
    STable headers rows ->
      let cols = headers
          allRows = headers : rows
          widths = map (max 1) (colWidths (List.transpose (padTo (length cols) allRows)))
          renderRow r =
            T.intercalate "  " (zipWith pad widths (take (length cols) (r ++ repeat "")))
          headerLine = renderRow headers
          sepLine = T.intercalate "  " (map (\w -> T.replicate w "-") widths)
       in T.unlines (headerLine : sepLine : map renderRow rows)
  where
    padTo n rs = map (\r -> take n (r ++ repeat "")) rs

    colWidths :: [[T.Text]] -> [Int]
    colWidths cols = map (maximum . map (T.length . stripAnsi)) cols

    pad :: Int -> T.Text -> T.Text
    pad w t = t <> T.replicate (w - T.length (stripAnsi t)) " "

-- | Format file size for display (human readable)
formatSize :: Int64 -> T.Text
formatSize bytes
  | bytes >= 1073741824 = T.pack $ printf "%.1fG" (fromIntegral bytes / 1073741824 :: Double)
  | bytes >= 1048576 = T.pack $ printf "%.1fM" (fromIntegral bytes / 1048576 :: Double)
  | bytes >= 1024 = T.pack $ printf "%.1fK" (fromIntegral bytes / 1024 :: Double)
  | otherwise = T.pack $ show bytes <> "B"

-- | Add visual indicator for file type
formatKind :: Bool -> Bool -> String -> T.Text
formatKind isDir isExec name
  | isDir = "\ESC[1;34m" <> T.pack name <> "/" <> "\ESC[0m" -- Bold Blue for directories
  | isExec = "\ESC[1;32m" <> T.pack name <> "*" <> "\ESC[0m" -- Bold Green for executables
  | otherwise = T.pack name

lsStructured :: LsOptions -> FilePath -> IO StructuredValue
lsStructured opts dirPath0 = do
  dirPath <- expandTilde dirPath0
  listResult <- try (Dir.listDirectory dirPath) :: IO (Either IOException [FilePath])
  case listResult of
    Left e -> pure (SText ("ls: " <> T.pack (show e)))
    Right allNames -> do
      -- Filter hidden files unless -a flag is set
      let names =
            if lsShowAll opts
              then allNames
              else filter (not . List.isPrefixOf ".") allNames
      let sorted = List.sort names
      rowResults <- mapM (mkRow dirPath) sorted
      let rows = [r | Right r <- rowResults] -- skip entries that failed
      -- Sort by size or time if requested
      let sortedRows = applySorting opts rows
      -- Remove the internal mtime column before returning (it's index 4)
      let visibleRows = map (take 4) sortedRows
      pure (STable ["name", "kind", "size", "permissions"] visibleRows)
  where
    mkRow dirPath n = do
      let fullPath = dirPath FP.</> n
      result <- try (Posix.getFileStatus fullPath) :: IO (Either IOException Posix.FileStatus)
      case result of
        Left _ -> do
          -- On error, produce a row with error indicators
          isDirResult <- try (Dir.doesDirectoryExist fullPath) :: IO (Either IOException Bool)
          let isDir = either (const False) id isDirResult
              displayName = formatKind isDir False n <> " [error]"
          pure (Right [displayName, "-", "-", "-", "0"])
        Right st -> do
          isDir <- Dir.doesDirectoryExist fullPath
          let kind = if isDir then "dir" else "file"
              sz = Posix.fileSize st
              sizeStr = if lsHumanReadable opts then formatSize (fromIntegral sz) else T.pack (show sz)
              perms = formatPermissions (Posix.fileMode st)
              isExec = not isDir && (Posix.ownerExecuteMode `Posix.intersectFileModes` (Posix.fileMode st) /= Posix.nullFileMode)
              displayName = formatKind isDir isExec n
              -- Store mtime as epoch seconds for sorting (hidden column)
              mtime = Posix.modificationTime st
              mtimeStr = T.pack (show mtime)
          pure (Right [displayName, T.pack kind, sizeStr, perms, mtimeStr])

    applySorting o rows
      | lsSortByTime o = sortByColumn 4 rows (lsReverse o) -- sort by mtime (column 4)
      | lsSortBySize o = sortByColumn 2 rows (lsReverse o) -- sort by size (column 2)
      | otherwise = if lsReverse o then reverse rows else rows

    sortByColumn idx rs rev =
      let sorted = List.sortBy (\a b -> compare (parseNum (a !! idx)) (parseNum (b !! idx))) rs
       in if rev then reverse sorted else sorted

    parseNum t = fromMaybe 0 (parseSizeBytes t)

-- | Format Unix permissions
formatPermissions :: FileMode -> T.Text
formatPermissions mode =
  T.pack $
    [ if Posix.ownerReadMode `Posix.intersectFileModes` mode /= Posix.nullFileMode then 'r' else '-',
      if Posix.ownerWriteMode `Posix.intersectFileModes` mode /= Posix.nullFileMode then 'w' else '-',
      if Posix.ownerExecuteMode `Posix.intersectFileModes` mode /= Posix.nullFileMode then 'x' else '-',
      if Posix.groupReadMode `Posix.intersectFileModes` mode /= Posix.nullFileMode then 'r' else '-',
      if Posix.groupWriteMode `Posix.intersectFileModes` mode /= Posix.nullFileMode then 'w' else '-',
      if Posix.groupExecuteMode `Posix.intersectFileModes` mode /= Posix.nullFileMode then 'x' else '-',
      if Posix.otherReadMode `Posix.intersectFileModes` mode /= Posix.nullFileMode then 'r' else '-',
      if Posix.otherWriteMode `Posix.intersectFileModes` mode /= Posix.nullFileMode then 'w' else '-',
      if Posix.otherExecuteMode `Posix.intersectFileModes` mode /= Posix.nullFileMode then 'x' else '-'
    ]

pwdStructured :: FilePath -> IO StructuredValue
pwdStructured dirPath = pure (SText (T.pack dirPath))

-- | @ps - process list
psStructured :: IO StructuredValue
psStructured = do
  result <- try (Proc.readCreateProcess (Proc.proc "ps" ["aux"]) "") :: IO (Either IOException String)
  case result of
    Left _ -> pure (SText "Error: could not read process list")
    Right output -> do
      let lns = lines output
      case lns of
        [] -> pure (STable ["PID", "USER", "%CPU", "%MEM", "COMMAND"] [])
        (_hdr : rest) -> do
          let rows = map parsePs rest
          pure (STable ["PID", "USER", "%CPU", "%MEM", "COMMAND"] rows)
  where
    parsePs line =
      let ws = words line
       in case ws of
            (user : pid : cpu : mem : _vsz : _rss : _tty : _stat : _start : _time : cmd) ->
              [T.pack pid, T.pack user, T.pack cpu, T.pack mem, T.pack (unwords cmd)]
            _ -> [T.pack line, "", "", "", ""]

-- | @df - disk space
dfStructured :: IO StructuredValue
dfStructured = do
  result <- try (Proc.readCreateProcess (Proc.proc "df" ["-h"]) "") :: IO (Either IOException String)
  case result of
    Left _ -> pure (SText "Error: could not read disk usage")
    Right output -> do
      let lns = lines output
      case lns of
        [] -> pure (STable ["Filesystem", "Size", "Used", "Avail", "Use%", "Mount"] [])
        (_hdr : rest) -> do
          let rows = map parseDf rest
          pure (STable ["Filesystem", "Size", "Used", "Avail", "Use%", "Mount"] rows)
  where
    -- Parse df line: first 5 fields are fs,size,used,avail,pct; remainder is mount path (may contain spaces)
    parseDf line =
      let ws = words line
       in case ws of
            (fs : size : used : avail : pct : mountParts) ->
              [T.pack fs, T.pack size, T.pack used, T.pack avail, T.pack pct, T.pack (unwords mountParts)]
            _ -> replicate 6 ""

-- | @env - environment variables
envStructured :: IO StructuredValue
envStructured = do
  env <- Env.getEnvironment
  let rows = map (\(k, v) -> [T.pack k, T.pack v]) (List.sortBy (\a b -> compare (fst a) (fst b)) env)
  pure (STable ["Variable", "Value"] rows)

-- | @find - find files
findStructured :: FilePath -> [T.Text] -> IO StructuredValue
findStructured basePath patterns = do
  path <- expandTilde basePath
  allFiles <- findFilesRecursive path
  let namePattern = if null patterns then "*" else T.unpack (head patterns)
  let matches = filter (matchGlobSimple namePattern . FP.takeFileName) allFiles
  let rows = map (\f -> [T.pack f]) (take 1000 matches) -- Limit results
  pure (STable ["path"] rows)

-- | Recursively find files, avoiding symlink loops and handling permission errors
findFilesRecursive :: FilePath -> IO [FilePath]
findFilesRecursive dir = findFilesRecursive' Set.empty dir

-- | Internal recursive find with visited set to avoid symlink cycles
findFilesRecursive' :: Set.Set (DeviceID, FileID) -> FilePath -> IO [FilePath]
findFilesRecursive' visited dir = do
  existsResult <- try (Dir.doesDirectoryExist dir) :: IO (Either IOException Bool)
  case existsResult of
    Left _ -> pure []
    Right False -> pure []
    Right True -> do
      listResult <- try (Dir.listDirectory dir) :: IO (Either IOException [FilePath])
      case listResult of
        Left _ -> pure [] -- Permission denied or other IO error
        Right entries -> do
          let fullPaths = map (dir FP.</>) entries
          subResults <- mapM (processEntry visited) fullPaths
          pure (concat subResults)
  where
    processEntry vis path = do
      -- Use getSymbolicLinkStatus to detect symlinks without following them
      statResult <- try (Posix.getSymbolicLinkStatus path) :: IO (Either IOException Posix.FileStatus)
      case statResult of
        Left _ -> pure [path] -- On error, just return the path without recursing
        Right st -> do
          let devIno = (Posix.deviceID st, Posix.fileID st)
          if Set.member devIno vis
            then pure [] -- Already visited, skip to avoid cycle
            else
              if Posix.isSymbolicLink st
                then pure [path] -- It's a symlink, include it but don't descend
                else
                  if Posix.isDirectory st
                    then do
                      let newVis = Set.insert devIno vis
                      subFiles <- findFilesRecursive' newVis path
                      pure (path : subFiles)
                    else pure [path]

matchGlobSimple :: String -> String -> Bool
matchGlobSimple "*" _ = True
matchGlobSimple pat str = matchGlob pat str

-- | Simple glob pattern matching
matchGlob :: String -> String -> Bool
matchGlob pat str = go pat str
  where
    go [] [] = True
    go [] _ = False
    go ('*' : ps) s = any (go ps) (suffixes s)
    go ('?' : ps) (_ : ss) = go ps ss
    go ('?' : _) [] = False
    go (p : ps) (c : cs) = p == c && go ps cs
    go _ [] = False

    suffixes :: String -> [String]
    suffixes s =
      s : case s of
        [] -> []
        (_ : xs) -> suffixes xs

-- | @sort - sort table by column
sortStructured :: T.Text -> StructuredValue -> Either T.Text StructuredValue
sortStructured colExpr v =
  case v of
    SText _ -> Left "sort: expected table input"
    STable headers rows -> do
      let colRaw = T.strip $ T.dropWhile (== '.') colExpr
          col = if colRaw == "permission" then "permissions" else colRaw
      idx <- maybe (Left ("sort: unknown column: " <> col)) Right (List.elemIndex col headers)
      let sorted = List.sortBy (\a b -> compare (stripAnsi (a !! idx)) (stripAnsi (b !! idx))) rows
      pure (STable headers sorted)

-- | select - select specific columns
selectStructured :: [T.Text] -> StructuredValue -> Either T.Text StructuredValue
selectStructured cols v =
  case v of
    SText _ -> Left "select: expected table input"
    STable headers rows -> do
      let resolveName n = if n == "permission" then "permissions" else n
          colNames = map (resolveName . T.strip . T.dropWhile (== '.')) cols
      indices <- mapM (\c -> maybe (Left ("select: unknown column: " <> c)) Right (List.elemIndex c headers)) colNames
      -- Validate all rows have enough columns
      let requiredMaxIndex = if null indices then 0 else maximum indices
      case findShortRow requiredMaxIndex 0 rows of
        Just (rowNum, rowLen) ->
          Left ("select: row " <> T.pack (show rowNum) <> " too short (has " <> T.pack (show rowLen) <> " columns, need " <> T.pack (show (requiredMaxIndex + 1)) <> ")")
        Nothing -> do
          let newHeaders = map (headers !!) indices
              newRows = map (\r -> map (r !!) indices) rows
          pure (STable newHeaders newRows)
  where
    findShortRow :: Int -> Int -> [[T.Text]] -> Maybe (Int, Int)
    findShortRow _ _ [] = Nothing
    findShortRow maxIdx rowNum (r : rs)
      | length r <= maxIdx = Just (rowNum, length r)
      | otherwise = findShortRow maxIdx (rowNum + 1) rs

expandTilde :: FilePath -> IO FilePath
expandTilde p
  | p == "~" || p == "~/" = Dir.getHomeDirectory
  | List.isPrefixOf "~/" p = do
      home <- Dir.getHomeDirectory
      pure (home FP.</> drop 2 p)
  | otherwise = pure p
