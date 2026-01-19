module Aeth.Types
  ( OutputMode (..),
    Segment (..),
    Pipeline (..),
    ShellState (..),
    BackgroundJob (..),
    JobStatus (..),
    emptyShellState,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import System.Posix.Types (ProcessID)

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

-- | Job status for background processes
data JobStatus = Running | Stopped | Done
  deriving (Eq, Show)

-- | Background job information
data BackgroundJob = BackgroundJob
  { jobId :: Int,
    jobPid :: ProcessID,
    jobCommand :: T.Text,
    jobStatus :: JobStatus
  }
  deriving (Eq, Show)

data ShellState = ShellState
  { cwd :: FilePath,
    baseEnv :: Map.Map String String,
    envOverrides :: Map.Map String String,
    deletedEnv :: Set.Set String, -- Tombstone set for unset variables
    lastExitCode :: Int,
    lastDurationMs :: Maybe Int,
    history :: [String],
    backgroundJobs :: [BackgroundJob],
    nextJobId :: Int
  }
  deriving (Eq, Show)

emptyShellState :: FilePath -> Map.Map String String -> ShellState
emptyShellState initialCwd base =
  ShellState
    { cwd = initialCwd,
      baseEnv = base,
      envOverrides = Map.singleton "PWD" initialCwd,
      deletedEnv = Set.empty,
      lastExitCode = 0,
      lastDurationMs = Nothing,
      history = [],
      backgroundJobs = [],
      nextJobId = 1
    }
