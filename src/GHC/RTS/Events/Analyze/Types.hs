{-# LANGUAGE TemplateHaskell #-}
module GHC.RTS.Events.Analyze.Types (
    EventId(..)
  , Options(..)
  , TimelineGranularity(..)
  , AnalysisState(..)
  , runningThreads
  , windowAnalyses
  , EventAnalysis(..)
  , events
  , windowThreadInfo
  , openEvents
  , startup
  , shutdown
  , inWindow
  , ThreadInfo
  , RunningThreads
  , threadIds
  , Quantized(..)
  , SortIndex
  , showEventId
  , isUserEvent
  , isThreadEvent
  ) where

import Control.Lens (makeLenses)
import Data.Map (Map)
import GHC.RTS.Events (Timestamp, ThreadId)
import qualified Data.Map as Map

-- | Event identifiers
--
-- The order of the constructors matters because it dictates the default
-- ordering in the output SVG
data EventId =
    -- | Garbage collection
    EventGC

    -- | User events
    --
    -- To use user events, do
    --
    -- > traceEventIO "START <label>"
    -- > ...
    -- > traceEventIO "STOP <label>"
  | EventUser String SortIndex

    -- | Threads
  | EventThread ThreadId
  deriving (Eq, Ord, Show)

-- | Sort index
--
-- Events can have an optional sort index, which can be used to control the
-- order in which events are sorted (the sort index itself is not shown
-- in the reports). To use this, simply use
--
-- > traceEventIO "START <sortIndex> <label>"
-- > ...
-- > traceEventIO "STOP <sortIndex> <label>"
type SortIndex = Int

data TimelineGranularity = TimelineSeconds | TimelineMilliseconds
  deriving Show

-- | Command line options
data Options = Options {
    optionsGenerateTimedSVG   :: Bool
  , optionsGenerateTimedText  :: Bool
  , optionsGenerateTotalsText :: Bool
  , optionsWindowEvent        :: String
  , optionsNumBuckets         :: Int
  , optionsUserStart          :: String
  , optionsUserStop           :: String
  , optionsScriptTotals       :: FilePath  -- "" denotes the standard script
  , optionsScriptTimed        :: FilePath
  , optionsGranularity        :: TimelineGranularity
  , optionsTickEvery          :: Int
  , optionsBucketWidth        :: Double
  , optionsBucketHeight       :: Double
  , optionsBorderWidth        :: Double -- '0' for no border
    -- Defined last to make defining the parser easier
  , optionsInput              :: FilePath
  }
  deriving Show

-- | Map of currently running threads to their labels
type RunningThreads = Map ThreadId String

threadIds :: RunningThreads -> [ThreadId]
threadIds = Map.keys

-- | Information about each thread to be stored per window
--
-- When was the thread created, when was it destroyed, and what is the
-- thread label (as indicated by ThreadLabel events)
--
-- The default label for each thread is the thread ID
type ThreadInfo = Map ThreadId (Timestamp, Timestamp, String)

-- | Analysis of a window (or the whole program run if there aren't any).
-- The fields that we use as "accumulators" in `analyze` are strict so that we
-- don't build up chains of EventAnalysis objects when we update it as we
-- process the eventlog.
data EventAnalysis = EventAnalysis {
    -- | Start and stop timestamps
    --
    -- For events that miss an end marker the @stop@ timestamp will be set to
    -- be equal to the @start@ timestamp
    _events :: ![(EventId, Timestamp, Timestamp)]

    -- | Window-specific thread info
  , _windowThreadInfo :: ThreadInfo

    -- | Event with a start but with a missing end.
    --
    -- Some events may be self-overlapping
    --
    -- > Start <event>
    -- > ..
    -- > Start <event>
    -- > ..
    -- >    End <event>
    -- > ..
    -- >    End <event>
    --
    -- (this happens in particular for GC events because GC events are listed
    -- separately for separate HECs). We therefore record for each open event
    -- how many start events we have seen, and hence how many ends we need to
    -- see before counting the event as finished.
  , _openEvents :: !(Map EventId (Timestamp, Int))

    -- | Total amount of time per event (non-strict)
  , eventTotals :: Map EventId Timestamp
  , eventStarts :: Map EventId Timestamp

    -- | Timestamp of the Startup event
  , _startup :: !(Maybe Timestamp)

    -- | Timestamp of the Shutdown event
  , _shutdown :: !(Maybe Timestamp)
  , _inWindow :: Bool
  }
  deriving Show

$(makeLenses ''EventAnalysis)

-- | State while running an analysis. Keeps track of currently running threads,
-- and appends an 'EventAnalysis' per window.
data AnalysisState = AnalysisState {
    _runningThreads :: RunningThreads
  , _windowAnalyses :: [EventAnalysis]
}

$(makeLenses ''AnalysisState)

-- | Quantization splits the total time up into @n@ buckets. We record for each
-- event and each bucket what percentage of that bucket the event used. A
-- missing entry denotes 0.
--
-- Quantization is essential because each individual event period might be too
-- small to show
--
-- For any given bucket the sum of all threads for that bucket cannot exceed the
-- number of cores.
data Quantized = Quantized {
    -- | For each event and each bucket how much of that bucket the event used up
    quantTimes      :: Map EventId (Map Int Double)
    -- | Like threadInfo, but quantized (start and finish bucket)
  , quantThreadInfo :: Map ThreadId (Int, Int, String)
    -- | Size of each bucket
  , quantBucketSize :: Timestamp
  }
  deriving Show

-- | Show an event ID given the specified options (for renaming events)
-- and information about threads (either `__threadInfo` from `EventAnalysis` or
-- `quantThreadInfo` from `Quantized`).
showEventId :: Map ThreadId (a, a, String) -> EventId -> String
showEventId _     EventGC          = "GC"
showEventId _    (EventUser event _) = event
showEventId info (EventThread tid) = case Map.lookup tid info of
                                       Just (_, _, l) -> l
                                       Nothing        -> show tid

isUserEvent :: EventId -> Bool
isUserEvent (EventUser{}) = True
isUserEvent _             = False

isThreadEvent :: EventId -> Bool
isThreadEvent (EventThread _) = True
isThreadEvent _               = False
