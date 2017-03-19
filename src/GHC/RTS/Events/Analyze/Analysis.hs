{-# LANGUAGE FlexibleContexts, CPP #-}
module GHC.RTS.Events.Analyze.Analysis (
    -- * Auxiliary
    readEventLog
    -- * Basic analysis
  , events
  , analyze
    -- * Using EventAnalysis
  , eventTotal
  , compareEventIds
    -- * Quantization
  , quantize
  ) where

import Prelude hiding (log)
import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (forM_, when, void)
import Data.Maybe (fromMaybe, isNothing)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import GHC.RTS.Events hiding (events)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import GHC.RTS.Events.Analyze.Utils
import GHC.RTS.Events.Analyze.StrictState (State, execState, put, get, runState)
import GHC.RTS.Events.Analyze.Types
import GHC.RTS.Events.Analyze.Script

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

sortedEvents :: EventLog -> [Event]
sortedEvents (EventLog _header (Data es)) = map ce_event (sortEvents es)

readEventLog :: FilePath -> IO EventLog
readEventLog  = throwLeftStr . readEventLogFromFile

{-------------------------------------------------------------------------------
  Basic analysis of the eventlog, making the information more easily accessible.
  In particular, many events come in pairs (start thread/end thread, etc.);
  the analysis combines such events.
-------------------------------------------------------------------------------}

analyze :: Options -> EventLog -> [EventAnalysis]
analyze opts@Options{..} log =
    let AnalysisState _ analyses = execState (mapM_ analyzeEvent (sortedEvents log))
                                             (initialAnalysisState opts)
    in reverse
       [ analysis { eventTotals = computeTotals (_events analysis)
                  , eventStarts = computeStarts (_events analysis) }
       | analysis <- (if length analyses > 1 then drop 1 else id) analyses ]
  where
    isWindowEvent :: EventId -> Bool
    isWindowEvent = case optionsWindowEvent of
                      Nothing -> const False
                      Just ev -> (== ev)

    analyzeEvent :: Event -> State AnalysisState ()
    analyzeEvent (Event time spec) = do
      cur $ recordShutdown time
      case spec of
        -- CapCreate/CapDelete are the "new" events (ghc >= 7.6)
        -- Startup/Shutdown are older (to support older eventlogs)
        CapCreate _cap             -> cur $ recordStartup  time
        CapDelete _cap             -> cur $ recordShutdown time
        Startup _numCaps           -> cur $ recordStartup  time
        Shutdown                   -> cur $ recordShutdown time
        -- Thread info
        CreateThread tid           -> recordThreadCreation tid time
        (finishThread -> Just tid) -> recordThreadFinish tid time
        -- Start/end events
        ThreadLabel tid l          -> labelThread tid l
        (startId -> Just eid)      -> do cur $ ifInWindow $ recordEventStart eid time
                                         when (isWindowEvent eid) $ recordWindowStart time
        (stopId  -> Just eid)      -> do when (isWindowEvent eid) $ recordWindowStop opts time
                                         cur $ ifInWindow $ recordEventStop eid time
        _                          -> return ()

    startId :: EventInfo -> Maybe EventId
    startId (RunThread tid)                                   = Just $ EventThread tid
    startId StartGC                                           = Just $ EventGC
    startId (UserMessage (prefix optionsUserStart -> Just e)) = Just $ parseUserEvent e
    startId _                                                 = Nothing

    stopId :: EventInfo -> Maybe EventId
    stopId (StopThread tid _)                               = Just $ EventThread tid
    stopId EndGC                                            = Just $ EventGC
    stopId (UserMessage (prefix optionsUserStop -> Just e)) = Just $ parseUserEvent e
    stopId _                                                = Nothing

ifInWindow :: State EventAnalysis () -> State EventAnalysis ()
ifInWindow m = do
  b <- use inWindow
  when b m

-- Lift actions on the current analysis to the head of the list.
cur :: State EventAnalysis a -> State AnalysisState a
cur m = do
  AnalysisState ts (h:t) <- get
  case runState m h of
    (r, h') -> put (AnalysisState ts (h':t)) >> return r

-- We take the _first_ CapCreate to be the official startup time
recordStartup :: Timestamp -> State EventAnalysis ()
recordStartup time = startup %= (<|> Just time)

-- We take the last time of any event to be the official shutdown time
recordShutdown :: Timestamp -> State EventAnalysis ()
recordShutdown time =
    shutdown %= (\prevt'm -> let newtime = maybe time (max time) prevt'm in newtime `seq` Just newtime)

recordEventStart :: EventId -> Timestamp -> State EventAnalysis ()
recordEventStart eid start = do
    oldValue <- openEvents . at eid <<%= Just . push (start,1)
    case (eid, oldValue) of
      -- Pretend user events stop on the _first_ StartGC
      (EventGC, Nothing) -> simulateUserEventsStopAt start
      _                  -> return ()
  where
    push _new Nothing = _new
    push (_newStart, _newCount) (Just(oldStart, oldCount)) =
      -- _newCount will always be 1; _newStart is irrelevant
      let count' = oldCount + 1
      in count' `seq` (oldStart, count')

recordEventStop :: EventId -> Timestamp -> State EventAnalysis ()
recordEventStop eid stop = do
    oldValue <- openEvents . at eid <<%= (>>= pop)
    case oldValue of
      Just (start, 1) -> do
        events     %= (:) (eid, start, stop)
        when (eid == EventGC) $ simulateUserEventsStartAt stop
      _ ->
        return ()
  where
    pop (_start, 1) = Nothing
    pop (start, count) =
      let count' = count - 1
      in count' `seq` Just (start, count')

simulateUserEventsStopAt :: Timestamp -> State EventAnalysis ()
simulateUserEventsStopAt stop = do
    nowOpen <- itoList <$> use openEvents
    forM_ nowOpen $ \(eid, (start, _count)) -> case eid of
      EventGC       -> return ()
      EventThread _ -> return ()
      EventUser _ _ -> events %= (:) (eid, start, stop)

simulateUserEventsStartAt :: Timestamp -> State EventAnalysis ()
simulateUserEventsStartAt newStart = openEvents %= Map.mapWithKey updUserEvent
  where
    updUserEvent :: EventId -> (Timestamp, Int) -> (Timestamp, Int)
    updUserEvent eid (oldStart, count) = case eid of
      EventGC       -> (oldStart, count)
      EventThread _ -> (oldStart, count)
      EventUser _ _ -> (newStart, count)

recordWindowStart :: Timestamp -> State AnalysisState ()
recordWindowStart time = do
  cur $ do
    startup .= Just time
    inWindow .= True
  -- Record creation of any threads that
  -- were running before window was entered
  recordRunningThreadCreation time

recordWindowStop :: Options -> Timestamp -> State AnalysisState ()
recordWindowStop opts time = do
  cur $ do
    inWindow .= False
    recordShutdown time
  recordRunningThreadFinish time
  windowAnalyses %= (initialEventAnalysis opts :)

-- Record thread creation in current window, and add it to the map of running threads
recordThreadCreation :: ThreadId -> Timestamp -> State AnalysisState ()
recordThreadCreation tid start = do
    let label = show tid
    cur $ ifInWindow $ recordWindowThreadCreation tid start [label]
    runningThreads . at tid .= Just [label]

-- Record thread creation in current window
recordWindowThreadCreation :: ThreadId -> Timestamp -> [String] -> State EventAnalysis ()
recordWindowThreadCreation tid start label =
    windowThreadInfo . at tid .=  Just (start, start, label)

-- Record the creation of all running threads in the current window
-- This should be used when entering a window
recordRunningThreadCreation :: Timestamp -> State AnalysisState ()
recordRunningThreadCreation start = do
    threads <- use runningThreads
    void $ Map.traverseWithKey recordWindowCreation threads
  where
    recordWindowCreation tid label = cur $ recordWindowThreadCreation tid start label

recordThreadFinish :: ThreadId -> Timestamp -> State AnalysisState ()
recordThreadFinish tid stop = do
    -- The "thread finished" doubles as a "thread stop"
    cur $ ifInWindow $ recordEventStop (EventThread tid) stop
    cur $ ifInWindow $ recordWindowThreadFinish tid stop
    runningThreads . at tid .= Nothing

recordWindowThreadFinish :: ThreadId -> Timestamp -> State EventAnalysis ()
recordWindowThreadFinish tid stop =
    windowThreadInfo . at tid %= fmap updStop
  where
    updStop (start, _stop, l) = (start, stop, l)

recordRunningThreadFinish :: Timestamp -> State AnalysisState ()
recordRunningThreadFinish stop = do
    threads <- use runningThreads
    mapM_ (\tid -> cur $ recordWindowThreadFinish tid stop) $ threadIds threads

labelThread :: ThreadId -> String -> State AnalysisState ()
labelThread tid l = do
    runningThreads . at tid %= fmap (consS l)
    cur $ windowThreadInfo . at tid %= fmap updThreadInfo
  where
    updThreadInfo (start, stop, l') = let !ll = consS l l' in (start, stop, ll)
    consS !a !b = a : b

finishThread :: EventInfo -> Maybe ThreadId
finishThread (StopThread tid ThreadFinished) = Just tid
finishThread _                               = Nothing

initialAnalysisState :: Options -> AnalysisState
initialAnalysisState opts = AnalysisState {
    _runningThreads = Map.empty
  , _windowAnalyses = [initialEventAnalysis opts]
  }

initialEventAnalysis :: Options -> EventAnalysis
initialEventAnalysis opts = EventAnalysis {
    _events           = []
  , _windowThreadInfo = Map.empty
  , _openEvents       = Map.empty
  , eventTotals       = error "eventTotals computed at the end"
  , eventStarts       = error "eventStarts computed at the end"
  , _startup          = Nothing
  , _shutdown         = Nothing
  , _inWindow         = isNothing (optionsWindowEvent opts)
  }

computeTotals :: [(EventId, Timestamp, Timestamp)] -> HashMap EventId Timestamp
computeTotals = go mempty
  where
    go :: HashMap EventId Timestamp
       -> [(EventId, Timestamp, Timestamp)]
       -> HashMap EventId Timestamp
    go !acc [] = acc
    go !acc ((eid, start, stop) : es) =
      go (Map.insertWith (+) eid (stop - start) acc) es

computeStarts :: [(EventId, Timestamp, Timestamp)] -> HashMap EventId Timestamp
computeStarts = go Map.empty
  where
    go :: HashMap EventId Timestamp
       -> [(EventId, Timestamp, Timestamp)]
       -> HashMap EventId Timestamp
    go !acc [] = acc
    go !acc ((eid, start, _) : es) =
      go (Map.insertWith min eid start acc) es

{-------------------------------------------------------------------------------
  Using EventAnalysis
-------------------------------------------------------------------------------}

-- | Lookup start time for a given event
eventStart :: EventAnalysis -> EventId -> Timestamp
eventStart EventAnalysis{..} eid =
    case Map.lookup eid eventStarts of
      Nothing -> error $ "eventStart: Invalid event ID " ++ show eid ++ ". "
                      ++ "Valid IDs are " ++ show (Map.keys eventStarts)
      Just t  -> t

-- | Lookup a total for a given event
eventTotal :: EventAnalysis -> EventId -> Timestamp
eventTotal EventAnalysis{..} eid = fromMaybe 0 $ Map.lookup eid eventTotals

-- | Compare event IDs
compareEventIds :: EventAnalysis -> EventSort
                -> EventId -> EventId -> Ordering
compareEventIds analysis sort a b =
    case sort of
      SortByName  -> compare a b
      SortByTotal -> compare (eventTotal analysis b) (eventTotal analysis a)
      SortByStart -> compare (eventStart analysis a) (eventStart analysis b)

{-------------------------------------------------------------------------------
  Quantization
-------------------------------------------------------------------------------}

quantize :: Int -> EventAnalysis -> Quantized
quantize numBuckets EventAnalysis{..} = Quantized {
      quantTimes      = go Map.empty _events
    , quantThreadInfo = Map.map quantizeThreadInfo _windowThreadInfo
    , quantBucketSize = bucketSize
    }
  where
    go :: HashMap EventId (IntMap Double)
       -> [(EventId, Timestamp, Timestamp)]
       -> HashMap EventId (IntMap Double)
    go !acc [] = acc
    go !acc ((eid, start, end) : ttimes') =
      let startBucket, endBucket :: Int
          startBucket = bucket start
          endBucket   = bucket end

          updates :: IntMap Double
          updates = IntMap.fromAscList
                  $ [ (b, delta startBucket endBucket start end b)
                    | b <- [startBucket .. endBucket]
                    ]

          update :: Maybe (IntMap Double) -> Maybe (IntMap Double)
          update Nothing    = Just $ updates
          update (Just old) = let new = IntMap.unionWith (+) updates old
                              in new `seq` Just new

      in go (Map.alter update eid acc) ttimes'

    --           (a,                    b)
    --       |          |   ...    |          |
    --        startBucket             endBucket
    --
    --                      ^^^
    --                      bucket

    delta :: Int -> Int -> Timestamp -> Timestamp -> Int -> Double
    delta startBucket endBucket start end b
      | b == startBucket && startBucket == endBucket =
         t2d (end - start) / t2d bucketSize
      | b == startBucket =
         t2d (bucketEnd b - start) / t2d bucketSize
      | b == endBucket =
         t2d (end - bucketStart b) / t2d bucketSize
      | otherwise =
         1

    startTime, endTime, bucketSize :: Timestamp
    startTime  = fromMaybe (error "_startup not set")  _startup
    endTime    = fromMaybe (error "_shutdown not set") _shutdown
    bucketSize = (endTime - startTime) `div` fromIntegral numBuckets

    bucketStart, bucketEnd :: Int -> Timestamp
    bucketStart b = startTime + fromIntegral b * bucketSize
    bucketEnd   b = bucketStart (b + 1)

    bucket :: Timestamp -> Int
    bucket t = fromIntegral ((t - startTime) `div` bucketSize)

    t2d :: Timestamp -> Double
    t2d = fromInteger . toInteger

    quantizeThreadInfo :: (Timestamp, Timestamp, a) -> (Int, Int, a)
    quantizeThreadInfo (start, stop, label) = (bucket start, bucket stop, label)
