{-# LANGUAGE OverloadedStrings #-}

module Aeth.LineEditor
  ( LineEditor,
    LineEditorSettings (..),
    defaultSettings,
    withLineEditor,
    getLineEdited,
  )
where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import System.Console.ANSI
  ( Color (..),
    ColorIntensity (..),
    ConsoleIntensity (..),
    ConsoleLayer (..),
    SGR (..),
    clearLine,
    cursorDownLine,
    cursorUpLine,
    getTerminalSize,
    hideCursor,
    setCursorColumn,
    setSGR,
    showCursor,
  )
import System.IO (BufferMode (NoBuffering), hFlush, hSetBuffering, hSetEcho, stdin, stdout)
import qualified System.Posix.IO as PIO
import qualified System.Posix.Terminal as PT

-- Normal-terminal line editor:
-- - does NOT use an alternate screen
-- - highlights the input as you type
-- - shows a fish-like autosuggestion (from history) in faint text (if enabled)
-- - basic keys: left/right/home/end, backspace/delete, tab (accept suggestion), enter, ctrl-d

-- | Settings for the line editor
data LineEditorSettings = LineEditorSettings
  { leSyntaxHighlighting :: Bool, -- Enable syntax highlighting
    leAutoSuggestions :: Bool -- Enable fish-like auto-suggestions
  }
  deriving (Eq, Show)

-- | Default settings (all features enabled)
defaultSettings :: LineEditorSettings
defaultSettings =
  LineEditorSettings
    { leSyntaxHighlighting = True,
      leAutoSuggestions = True
    }

data LineEditor = LineEditor

withLineEditor :: (LineEditor -> m a) -> m a
withLineEditor action = action LineEditor

getLineEdited :: LineEditor -> LineEditorSettings -> [T.Text] -> String -> [String] -> IO (Maybe String)
getLineEdited _ settings _scrollback prompt history0 =
  withRawTerminal $ do
    putStr prompt
    hFlush stdout
    loop "" 0 0 Nothing ""
  where
    -- Newest-first history for navigation.
    history = reverse history0

    -- Helper to get suggestion (respects autoSuggestions setting)
    getSuggestion buf
      | leAutoSuggestions settings = suggestion buf history
      | otherwise = Nothing

    loop :: String -> Int -> Int -> Maybe Int -> String -> IO (Maybe String)
    loop buf cursor prevLines histIx savedBuf = do
      newPrevLines <- render buf cursor prevLines
      key <- readKey
      case key of
        KCtrlD ->
          if null buf then pure Nothing else loop buf cursor newPrevLines histIx savedBuf
        KEnter -> do
          putStr "\n"
          hFlush stdout
          pure (Just buf)
        KLeft -> loop buf (max 0 (cursor - 1)) newPrevLines histIx savedBuf
        KRight -> loop buf (min (length buf) (cursor + 1)) newPrevLines histIx savedBuf
        KHome -> loop buf 0 newPrevLines histIx savedBuf
        KEnd -> loop buf (length buf) newPrevLines histIx savedBuf
        KUp ->
          case history of
            [] -> loop buf cursor newPrevLines histIx savedBuf
            _ ->
              case histIx of
                Nothing ->
                  let newIx = 0
                      newBuf = history !! newIx
                   in loop newBuf (length newBuf) newPrevLines (Just newIx) buf
                Just ix ->
                  let newIx = min (length history - 1) (ix + 1)
                      newBuf = history !! newIx
                   in loop newBuf (length newBuf) newPrevLines (Just newIx) savedBuf
        KDown ->
          case histIx of
            Nothing -> loop buf cursor newPrevLines histIx savedBuf
            Just ix ->
              if ix <= 0
                then loop savedBuf (length savedBuf) newPrevLines Nothing savedBuf
                else
                  let newIx = ix - 1
                      newBuf = history !! newIx
                   in loop newBuf (length newBuf) newPrevLines (Just newIx) savedBuf
        KBackspace ->
          if cursor <= 0
            then loop buf cursor newPrevLines histIx savedBuf
            else loop (take (cursor - 1) buf <> drop cursor buf) (cursor - 1) newPrevLines histIx savedBuf
        KDelete ->
          if cursor >= length buf
            then loop buf cursor newPrevLines histIx savedBuf
            else loop (take cursor buf <> drop (cursor + 1) buf) cursor newPrevLines histIx savedBuf
        KTab ->
          case getSuggestion buf of
            Nothing -> loop buf cursor newPrevLines histIx savedBuf
            Just sug -> loop sug (length sug) newPrevLines histIx savedBuf
        KChar c ->
          let (a, b) = splitAt cursor buf
              newBuf = a <> [c] <> b
           in loop newBuf (cursor + 1) newPrevLines histIx savedBuf
        KText s ->
          let s' = sanitizePaste s
              (a, b) = splitAt cursor buf
              newBuf = a <> s' <> b
           in loop newBuf (cursor + length s') newPrevLines histIx savedBuf
        KUnknown -> loop buf cursor newPrevLines histIx savedBuf

    sanitizePaste :: String -> String
    sanitizePaste = map (\c -> if c == '\n' || c == '\r' then ' ' else c)

    render :: String -> Int -> Int -> IO Int
    render buf cursor prevLines = do
      -- Hide cursor during re-render to prevent flickering/blinking
      hideCursor
      -- Clear the previously-rendered wrapped lines.
      when (prevLines > 0) $ do
        _ <- cursorUpLine prevLines
        pure ()
      setCursorColumn 0
      clearLine
      -- Clear additional wrapped lines (if any).
      if prevLines <= 0
        then pure ()
        else do
          mapM_ (\_ -> cursorDownLine 1 >> setCursorColumn 0 >> clearLine) [1 .. prevLines]
          cursorUpLine prevLines
          setCursorColumn 0

      -- Draw prompt + buffer + suggestion.
      putStr prompt
      if leSyntaxHighlighting settings
        then renderHighlighted (T.pack buf)
        else putStr buf

      mSug <-
        case getSuggestion buf of
          Nothing -> pure Nothing
          Just sug -> do
            let rest = drop (length buf) sug
            setSGR [SetConsoleIntensity FaintIntensity]
            putStr rest
            setSGR [Reset]
            pure (Just rest)

      mSize <- getTerminalSize
      let cols = case mSize of Just (_rows, c) | c > 0 -> c; _ -> 80

      let promptCols = visibleWidth prompt
      let bufPrefixCols = visibleWidth (take cursor buf)
      let bufCols = visibleWidth buf
      let sugCols = maybe 0 visibleWidth mSug
      let totalCols = promptCols + bufCols + sugCols
      let endRow = if cols <= 0 then 0 else totalCols `div` cols

      let cursorCols = promptCols + bufPrefixCols
      let cursorRow = if cols <= 0 then 0 else cursorCols `div` cols
      let cursorCol = if cols <= 0 then cursorCols else cursorCols `mod` cols
      let upBy = endRow - cursorRow
      when (upBy > 0) $ do
        _ <- cursorUpLine upBy
        pure ()
      setCursorColumn cursorCol
      -- Show cursor now that it's positioned correctly
      showCursor
      hFlush stdout
      pure endRow

-- | Compute terminal column width of a string, ignoring ANSI escape sequences.
--
-- This keeps cursor alignment correct when the prompt contains colors (SGR) or other escapes.
-- Note: this is a pragmatic implementation; it counts codepoints as 1 column.
visibleWidth :: String -> Int
visibleWidth = go 0
  where
    go n [] = n
    go n ('\ESC' : xs) =
      case xs of
        ('[' : rest) -> go n (dropCsi rest)
        (']' : rest) -> go n (dropOsc rest)
        _ -> go n xs
    go n (_ : xs) = go (n + 1) xs

    -- CSI: ESC [ ... <final>
    -- Final byte is in range 0x40 ('@') .. 0x7E ('~')
    dropCsi :: String -> String
    dropCsi [] = []
    dropCsi (c : cs)
      | c >= '@' && c <= '~' = cs
      | otherwise = dropCsi cs

    -- OSC: ESC ] ... BEL  or ESC \
    dropOsc :: String -> String
    dropOsc [] = []
    dropOsc ('\BEL' : cs) = cs
    dropOsc ('\ESC' : '\\' : cs) = cs
    dropOsc (_ : cs) = dropOsc cs

renderHighlighted :: T.Text -> IO ()
renderHighlighted t = mapM_ renderTok (lexTokens t)
  where
    renderTok (TokSpace s) = setSGR [Reset] >> putStr (T.unpack s)
    renderTok (TokPipe s) = do
      setSGR [SetColor Foreground Vivid Yellow]
      putStr (T.unpack s)
      setSGR [Reset]
    renderTok (TokOperator s) = do
      setSGR [SetColor Foreground Vivid Yellow, SetConsoleIntensity BoldIntensity]
      putStr (T.unpack s)
      setSGR [Reset]
    renderTok (TokCmd s) = do
      let str = T.unpack s
      if "@" `isPrefixOf` str
        then setSGR [SetColor Foreground Vivid Green, SetConsoleIntensity BoldIntensity]
        else setSGR [SetColor Foreground Vivid Cyan, SetConsoleIntensity BoldIntensity]
      putStr str
      setSGR [Reset]
    renderTok (TokArg s) = setSGR [Reset] >> putStr (T.unpack s)
    renderTok (TokString s) = do
      setSGR [SetColor Foreground Vivid Green]
      putStr (T.unpack s)
      setSGR [Reset]
    renderTok (TokEnvVar s) = do
      setSGR [SetColor Foreground Vivid Red]
      putStr (T.unpack s)
      setSGR [Reset]
    renderTok (TokRedirect s) = do
      setSGR [SetColor Foreground Vivid Magenta]
      putStr (T.unpack s)
      setSGR [Reset]
    renderTok (TokPath s) = do
      setSGR [SetColor Foreground Dull Cyan]
      putStr (T.unpack s)
      setSGR [Reset]

suggestion :: String -> [String] -> Maybe String
suggestion prefix hs
  | all isSpace prefix = Nothing
  | otherwise =
      case filter (isPrefixOf prefix) hs of
        (x : _) -> Just x
        [] -> Nothing

data Tok
  = TokPipe T.Text
  | -- | and ||
    TokOperator T.Text -- &&, ;, &
  | TokSpace T.Text
  | TokCmd T.Text -- Commands (first word)
  | TokArg T.Text -- Normal arguments
  | TokString T.Text -- Quoted strings
  | TokEnvVar T.Text
  | -- \$VAR or ${VAR}
    TokRedirect T.Text -- <, >, >>, 2>
  | TokPath T.Text -- Paths starting with / or ./

lexTokens :: T.Text -> [Tok]
lexTokens = go True
  where
    go _ s | T.null s = []
    go isCmdPos s =
      case T.uncons s of
        -- Operators: && || ;
        Just ('&', rest)
          | Just ('&', rest2) <- T.uncons rest -> TokOperator "&&" : go True rest2
          | otherwise -> TokOperator "&" : go True rest
        Just (';', rest) -> TokOperator ";" : go True rest
        Just ('|', rest)
          | Just ('|', rest2) <- T.uncons rest -> TokPipe "||" : go True rest2
          | otherwise -> TokPipe "|" : go True rest
        -- Redirections: >> > < 2> 2>&1
        Just ('>', rest)
          | Just ('>', rest2) <- T.uncons rest -> TokRedirect ">>" : go isCmdPos rest2
          | otherwise -> TokRedirect ">" : go isCmdPos rest
        Just ('<', rest) -> TokRedirect "<" : go isCmdPos rest
        Just ('2', rest)
          | Just ('>', rest2) <- T.uncons rest ->
              case T.uncons rest2 of
                Just ('&', rest3) -> case T.uncons rest3 of
                  Just ('1', rest4) -> TokRedirect "2>&1" : go isCmdPos rest4
                  _ -> TokRedirect "2>" : go isCmdPos rest2
                _ -> TokRedirect "2>" : go isCmdPos rest2
          | otherwise ->
              let (w, rest') = breakToken s
                  tok = if isCmdPos then TokCmd w else TokArg w
               in tok : go False rest'
        -- Environment variables: $VAR or ${VAR}
        Just ('$', rest) ->
          let (var, rest2) = parseEnvVar rest
           in TokEnvVar (T.cons '$' var) : go isCmdPos rest2
        -- Quoted strings
        Just ('"', _) ->
          let (quoted, rest2) = parseQuoted '"' s
           in TokString quoted : go isCmdPos rest2
        Just ('\'', _) ->
          let (quoted, rest2) = parseQuoted '\'' s
           in TokString quoted : go isCmdPos rest2
        -- Whitespace
        Just (c, _)
          | isSpace c ->
              let (sp, rest) = T.span isSpace s
               in TokSpace sp : go isCmdPos rest
        -- Commands and arguments
        _ ->
          let (w, rest) = breakToken s
              tok
                | isCmdPos = TokCmd w
                | isPath w = TokPath w
                | otherwise = TokArg w
           in tok : go False rest

    -- Check if text looks like a path
    isPath :: T.Text -> Bool
    isPath t = case T.uncons t of
      Just ('/', _) -> True
      Just ('.', rest) -> case T.uncons rest of
        Just ('/', _) -> True
        _ -> False
      Just ('~', _) -> True
      _ -> False

    -- Parse environment variable name
    parseEnvVar :: T.Text -> (T.Text, T.Text)
    parseEnvVar t = case T.uncons t of
      Just ('{', rest) ->
        let (name, rest2) = T.break (== '}') rest
         in case T.uncons rest2 of
              Just ('}', rest3) -> (T.cons '{' (T.snoc name '}'), rest3)
              _ -> (T.cons '{' name, rest2)
      _ -> T.span isVarChar t

    isVarChar c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_'

    -- Parse quoted string including the quotes
    parseQuoted :: Char -> T.Text -> (T.Text, T.Text)
    parseQuoted q t = go' (T.unpack t) []
      where
        go' [] acc = (T.pack (reverse acc), T.empty)
        go' (c : cs) acc
          | c == q && null acc = go' cs [c] -- Opening quote
          | c == '\\' = case cs of
              (c2 : cs') -> go' cs' (c2 : '\\' : acc)
              [] -> (T.pack (reverse (c : acc)), T.empty)
          | c == q = (T.pack (reverse (c : acc)), T.pack cs) -- Closing quote
          | otherwise = go' cs (c : acc)

    breakToken :: T.Text -> (T.Text, T.Text)
    breakToken = T.break (\c -> isSpace c || c `elem` ("|&;<>\"'" :: String))

data Key
  = KChar Char
  | KText String
  | KEnter
  | KLeft
  | KRight
  | KUp
  | KDown
  | KHome
  | KEnd
  | KBackspace
  | KDelete
  | KTab
  | KCtrlD
  | KUnknown

readKey :: IO Key
readKey = do
  c <- getChar
  case c of
    '\n' -> pure KEnter
    '\r' -> pure KEnter
    '\t' -> pure KTab
    '\DEL' -> pure KBackspace
    '\BS' -> pure KBackspace
    '\EOT' -> pure KCtrlD
    '\ESC' -> readEscape
    _ -> pure (KChar c)
  where
    readEscape = do
      c1 <- getChar
      case c1 of
        -- CSI
        '[' -> readCsi
        -- SS3 (common for Home/End on some terms): ESC O H / ESC O F
        'O' -> do
          c2 <- getChar
          case c2 of
            'H' -> pure KHome
            'F' -> pure KEnd
            'A' -> pure KUp
            'B' -> pure KDown
            'C' -> pure KRight
            'D' -> pure KLeft
            _ -> pure KUnknown
        _ -> pure KUnknown

    readCsi = do
      c2 <- getChar
      case c2 of
        'A' -> pure KUp
        'B' -> pure KDown
        'D' -> pure KLeft
        'C' -> pure KRight
        'H' -> pure KHome
        'F' -> pure KEnd
        -- Delete: ESC [ 3 ~
        '3' -> do
          _ <- getChar -- '~'
          pure KDelete
        -- Bracketed paste: ESC [ 200 ~ ... ESC [ 201 ~
        '2' -> do
          digits <- readDigitsUntilTilde
          case digits of
            "00" -> KText <$> readBracketedPaste
            "~" -> pure KUnknown
            _ -> pure KUnknown
        -- Home/End variants: ESC [ 1 ~ / ESC [ 4 ~
        '1' -> do
          _ <- getChar -- '~'
          pure KHome
        '4' -> do
          _ <- getChar -- '~'
          pure KEnd
        _ -> pure KUnknown

    readDigitsUntilTilde :: IO String
    readDigitsUntilTilde = go []
      where
        go acc = do
          c <- getChar
          case c of
            '~' -> pure (reverse acc)
            _ -> go (c : acc)

    readBracketedPaste :: IO String
    readBracketedPaste = go []
      where
        -- Read until ESC[201~
        go acc = do
          c <- getChar
          if c == '\ESC'
            then do
              c1 <- getChar
              if c1 /= '['
                then go ('\ESC' : c1 : acc)
                else do
                  c2 <- getChar
                  if c2 == '2'
                    then do
                      digits <- readDigitsUntilTilde
                      if digits == "01"
                        then pure (reverse acc)
                        else go (reverse ("\ESC[2" <> digits <> "~") ++ acc)
                    else go (c2 : '[' : '\ESC' : acc)
            else go (c : acc)

withRawTerminal :: IO a -> IO a
withRawTerminal action =
  bracket setup restore (const action)
  where
    fd = PIO.stdInput
    setup = do
      oldAttrs <- PT.getTerminalAttributes fd
      let newAttrs =
            PT.withoutMode (PT.withoutMode oldAttrs PT.EnableEcho) PT.ProcessInput
      PT.setTerminalAttributes fd newAttrs PT.WhenFlushed
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout NoBuffering
      hSetEcho stdin False
      -- Enable bracketed paste mode (terminals that support it will wrap paste in ESC[200~..ESC[201~).
      putStr "\ESC[?2004h"
      hFlush stdout
      pure oldAttrs

    restore oldAttrs = do
      PT.setTerminalAttributes fd oldAttrs PT.WhenFlushed
      hSetEcho stdin True
      putStr "\ESC[?2004l"
      hFlush stdout
