module Aeth.Types
  ( OutputMode (..),
    Segment (..),
    Pipeline (..),
    ShellState (..),
    emptyShellState,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data OutputMode
  = RawString
  | Structured
  deriving (Eq, Show)

data Segment = Segment
  { segMode :: OutputMode,
    segName :: T.Text,
    segArgs :: [T.Text]
  }
  deriving (Eq, Show)

newtype Pipeline = Pipeline {unPipeline :: [Segment]}
  deriving (Eq, Show)

data ShellState = ShellState
  { cwd :: FilePath,
    envOverrides :: Map.Map String String,
    lastExitCode :: Int,
    lastDurationMs :: Maybe Int,
    history :: [String]
  }
  deriving (Eq, Show)

emptyShellState :: FilePath -> ShellState
emptyShellState initialCwd = ShellState {cwd = initialCwd, envOverrides = Map.empty, lastExitCode = 0, lastDurationMs = Nothing, history = []}
