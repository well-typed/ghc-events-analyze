{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.RTS.Events.Analyze.Types (
    -- * Events
    EventId(..)
  , EventLabel
  , EventSubscript
  , isUserEvent
  , isThreadEvent
  , parseUserEvent
  , showEventId
    -- * Options
  , TimelineGranularity(..)
  , Options(..)
    -- * Analysis state
  , RunningThreads
  , ThreadLabels
  , threadIds
  , ThreadInfo
  , EventAnalysis(..)
  , AnalysisState(..)
  , mkThreadFilter
    -- ** EventAnalysis lenses
  , events
  , windowThreadInfo
  , openEvents
  , startup
  , shutdown
  , inWindow
    -- ** AnalysisState lenses
  , runningThreads
  , windowAnalyses
    -- * Analysis result
  , Quantized(..)
  , ThreadId
  ) where

import Control.Lens
import Data.Char
import Data.Hashable
import Data.HashMap.Strict (HashMap)
import Data.IntMap.Strict (IntMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import GHC.Generics
import GHC.RTS.Events (Timestamp, ThreadId)
import Text.Regex.PCRE
import Text.Regex.PCRE.Text () -- for RegexLike Regex Text

import GHC.RTS.Events.Analyze.Utils (showThreadId)

{-------------------------------------------------------------------------------
  Event identifiers
-------------------------------------------------------------------------------}

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
    -- > traceEventIO "START <subscript> <label>"
    -- > ...
    -- > traceEventIO "STOP <subscript> <label>"
    --
    -- If the <subscript> is omitted it is assumed to be 0
    -- (see 'EventSubscript' for details).
  | EventUser EventLabel EventSubscript

    -- | Threads
  | EventThread ThreadId
  deriving (Eq, Generic, Hashable, Ord, Show)

-- | The user-readable name for an event
type EventLabel = Text

-- | Event subscript
--
-- The combination of the 'EventLabel' and the 'EventSubscript' uniquely
-- identifies an event; however, only the 'EventLabel' is shown in the report.
--
-- When it is not required, the subscript can simply be set to 0.
type EventSubscript = Int

isUserEvent :: EventId -> Bool
isUserEvent (EventUser{}) = True
isUserEvent _             = False

isThreadEvent :: EventId -> Maybe ThreadId
isThreadEvent (EventThread tid) = Just tid
isThreadEvent _                 = Nothing

-- | Parse user event
--
-- If the event name starts with a digit, regard it as a 'EventSubscript'.
parseUserEvent :: Text -> EventId
parseUserEvent s =
    case TR.signed TR.decimal s of
      Left _ -> EventUser s 0
      Right (eid, cs) -> EventUser (T.dropWhile isSpace cs) eid

-- | Show an event ID given thread info
--
-- The argument is typically either `__threadInfo` from `EventAnalysis` or
-- `quantThreadInfo` from `Quantized`.
showEventId :: HashMap ThreadId (a,b,Text) -> EventId -> Text
showEventId _    EventGC             = "GC"
showEventId _    (EventUser event _) = event
showEventId info (EventThread tid)   = case info ^. at tid of
                                         Just (_, _, l) -> l
                                         Nothing        -> showThreadId tid

{-------------------------------------------------------------------------------
  Options
-------------------------------------------------------------------------------}

data TimelineGranularity = TimelineSeconds | TimelineMilliseconds
  deriving Show

-- | Command line options
data Options = Options {
    optionsGenerateTimedSVG   :: Bool
  , optionsGenerateTimedText  :: Bool
  , optionsGenerateTotalsText :: Bool
  , optionsWindowEvent        :: Maybe EventId
  , optionsNumBuckets         :: Int
  , optionsUserStart          :: Text
  , optionsUserStop           :: Text
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

{-------------------------------------------------------------------------------
  Analysis state
-------------------------------------------------------------------------------}

-- Thread labels held by a thread (as indicated by ThreadLabel events)
-- in inverse chronological order (most recent first)
type ThreadLabels = [Text]

-- | Map of currently running threads to their labels
type RunningThreads = HashMap ThreadId ThreadLabels

threadIds :: RunningThreads -> [ThreadId]
threadIds = map fst . itoList

-- | Information about each thread to be stored per window
--
-- When was the thread created, when was it destroyed, and what are the
-- thread labels in inverse cronological order (as indicated by ThreadLabel events)
--
-- The default label for each thread is the thread ID
type ThreadInfo = HashMap ThreadId (Timestamp, Timestamp, ThreadLabels)

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
  , _openEvents :: !(HashMap EventId (Timestamp, Int))

    -- | Total amount of time per event (non-strict)
  , eventTotals :: HashMap EventId Timestamp
  , eventStarts :: HashMap EventId Timestamp

    -- | Timestamp of the Startup event
  , _startup :: !(Maybe Timestamp)

    -- | Timestamp of the Shutdown event
  , _shutdown :: !(Maybe Timestamp)
  , _inWindow :: Bool
  }
  deriving Show

$(makeLenses ''EventAnalysis)

mkThreadFilter :: ThreadInfo-> String -> ThreadId -> Bool
mkThreadFilter analysis regex =
  let r :: Regex = makeRegex regex
      m = analysis & each %~ (\(_,_,tn) -> any (matchTest r) tn)
  in \tid -> m ^?! ix tid

-- | State while running an analysis. Keeps track of currently running threads,
-- and appends an 'EventAnalysis' per window.
data AnalysisState = AnalysisState {
    _runningThreads :: RunningThreads
  , _windowAnalyses :: NonEmpty EventAnalysis
}

$(makeLenses ''AnalysisState)

{-------------------------------------------------------------------------------
  Analysis result
-------------------------------------------------------------------------------}

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
    quantTimes      :: HashMap EventId (IntMap Double)
    -- | Like threadInfo, but quantized (start and finish bucket)
  , quantThreadInfo :: HashMap ThreadId (Int, Int, ThreadLabels)
    -- | Size of each bucket
  , quantBucketSize :: Timestamp
  }
  deriving Show
