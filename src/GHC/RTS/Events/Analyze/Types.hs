{-# LANGUAGE TemplateHaskell #-}
module GHC.RTS.Events.Analyze.Types (
    EventId(..)
  , Options(..)
  , EventAnalysis(..)
  , events
  , threadInfo
  , openEvents
  , startup
  , shutdown
  , numThreads
  , Quantized(..)
  , showEventId
  , isUserEvent
  , isThreadEvent
  ) where

import Control.Lens (Lens', makeLenses, at, (^.))
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
  | EventUser String

    -- | Threads
  | EventThread ThreadId
  deriving (Eq, Ord, Show)

-- | Command line options
data Options = Options {
    optionsGenerateTimedSVG   :: Bool
  , optionsGenerateTimedText  :: Bool
  , optionsGenerateTotalsText :: Bool
  , optionsNumBuckets         :: Int
  , optionsUserStart          :: String
  , optionsUserStop           :: String
  , optionsScriptTotals       :: FilePath  -- "" denotes the standard script
  , optionsScriptTimed        :: FilePath
    -- Defined last to make defining the parser easier
  , optionsInput              :: FilePath
  }
  deriving Show

-- The fields that we use as "accumulators" in `analyze` are strict so that we
-- don't build up chains of EventAnalysis objects when we update it as we
-- process the eventlog
data EventAnalysis = EventAnalysis {
    -- | Start and stop timestamps
    --
    -- For events that miss an end marker the @stop@ timestamp will be set to
    -- be equal to the @start@ timestamp
    _events :: ![(EventId, Timestamp, Timestamp)]

    -- | Information about each thread
    --
    -- When was the thread created, when was it destroyed, and what is the
    -- thread label (as indicated by ThreadLabel events)
    --
    -- The default label for each thread is the thread ID
  , __threadInfo :: !(Map ThreadId (Timestamp, Timestamp, String))

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

    -- | Timestamp of the Startup event
  , _startup :: !(Maybe Timestamp)

    -- | Timestamp of the Shutdown event
  , _shutdown :: !(Maybe Timestamp)
  }
  deriving Show

$(makeLenses ''EventAnalysis)

threadInfo :: ThreadId -> Lens' EventAnalysis (Maybe (Timestamp, Timestamp, String))
threadInfo tid = _threadInfo . at tid

numThreads :: EventAnalysis -> Int
numThreads analysis = Map.size (analysis ^. _threadInfo)

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
showEventId _    (EventUser event) = event
showEventId info (EventThread tid) = case Map.lookup tid info of
                                       Just (_, _, l) -> l
                                       Nothing        -> show tid

isUserEvent :: EventId -> Bool
isUserEvent (EventUser _) = True
isUserEvent _             = False

isThreadEvent :: EventId -> Bool
isThreadEvent (EventThread _) = True
isThreadEvent _               = False
