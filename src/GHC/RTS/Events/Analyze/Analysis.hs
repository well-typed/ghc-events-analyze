module GHC.RTS.Events.Analyze.Analysis (
    -- * Auxiliary
    readEventLog
    -- * Basic analysis
  , events
  , threadInfo
  , numThreads
  , analyze
    -- * Totals
  , computeTotals
    -- * Quantization
  , quantize
  ) where

import Prelude hiding (id, log)
import Control.Applicative ((<$>))
import Control.Lens ((%=), (.=), use)
import Control.Monad (forM_, when)
import Data.Map (Map)
import GHC.RTS.Events hiding (events)
import qualified Data.Map as Map

import GHC.RTS.Events.Analyze.Utils
import GHC.RTS.Events.Analyze.StrictState (State, execState)
import GHC.RTS.Events.Analyze.Types

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

analyze :: Options -> EventLog -> EventAnalysis
analyze Options{..} log =
    execState (mapM_ analyzeEvent (sortedEvents log)) initialEventAnalysis
  where
    analyzeEvent :: Event -> State EventAnalysis ()
    analyzeEvent (Event time spec) = case spec of
        CreateThread tid           -> recordThreadCreation tid time
        (finishThread -> Just tid) -> recordThreadFinish tid time
        ThreadLabel tid l          -> labelThread tid l
        (startId -> Just eid)      -> recordEventStart eid time
        (stopId  -> Just eid)      -> recordEventStop eid time
        _                          -> return ()

    startId :: EventInfo -> Maybe EventId
    startId (RunThread tid)                                   = Just $ EventThread tid
    startId StartGC                                           = Just $ EventGC
    startId (UserMessage (prefix optionsUserStart -> Just e)) = Just $ EventUser e
    startId _                                                 = Nothing

    stopId :: EventInfo -> Maybe EventId
    stopId (StopThread tid _)                               = Just $ EventThread tid
    stopId EndGC                                            = Just $ EventGC
    stopId (UserMessage (prefix optionsUserStop -> Just e)) = Just $ EventUser e
    stopId _                                                = Nothing

recordEventStart :: EventId -> Timestamp -> State EventAnalysis ()
recordEventStart eid start = do
    (oldValue, newOpen) <- Map.insertLookupWithKey' push eid (start, 1) <$> use openEvents
    openEvents .= newOpen
    case (eid, oldValue) of
      -- Pretend user events stop on the _first_ StartGC
      (EventGC, Nothing) -> simulateUserEventsStopAt start
      _                  -> return ()
  where
    push _ (_newStart, _newCount) (oldStart, oldCount) =
      -- _newCount will always be 1; _newStart is irrelevant
      let count' = oldCount + 1
      in count' `seq` (oldStart, count')

recordEventStop :: EventId -> Timestamp -> State EventAnalysis ()
recordEventStop eid stop = do
    (newValue, newOpen) <- Map.updateLookupWithKey pop eid <$> use openEvents
    case newValue of
      Just (start, 0) -> do
        openEvents %= Map.delete eid
        events     %= (:) (eid, start, stop)
        when (eid == EventGC) $ simulateUserEventsStartAt stop
      _ ->
        openEvents .= newOpen
  where
    pop _ (start, count) =
      let count' = count - 1
      in count' `seq` Just (start, count')

simulateUserEventsStopAt :: Timestamp -> State EventAnalysis ()
simulateUserEventsStopAt stop = do
    nowOpen <- Map.toList <$> use openEvents
    forM_ nowOpen $ \(eid, (start, _count)) -> case eid of
      EventGC       -> return ()
      EventThread _ -> return ()
      EventUser _   -> events %= (:) (eid, start, stop)

simulateUserEventsStartAt :: Timestamp -> State EventAnalysis ()
simulateUserEventsStartAt newStart = openEvents %= Map.mapWithKey updUserEvent
  where
    updUserEvent :: EventId -> (Timestamp, Int) -> (Timestamp, Int)
    updUserEvent eid (oldStart, count) = case eid of
      EventGC       -> (oldStart, count)
      EventThread _ -> (oldStart, count)
      EventUser _   -> (newStart, count)

recordThreadCreation :: ThreadId -> Timestamp -> State EventAnalysis ()
recordThreadCreation tid start =
    threadInfo tid .= Just (start, start, show tid)

recordThreadFinish :: ThreadId -> Timestamp -> State EventAnalysis ()
recordThreadFinish tid stop = do
    -- The "thread finished" doubles as a "thread stop"
    recordEventStop (EventThread tid) stop
    threadInfo tid %= fmap updStop
  where
    updStop (start, _stop, l) = (start, stop, l)

labelThread :: ThreadId -> String -> State EventAnalysis ()
labelThread tid l =
    threadInfo tid %= fmap updLabel
  where
    updLabel (start, stop, l') = (start, stop, l ++ " (" ++ l' ++ ")")

finishThread :: EventInfo -> Maybe ThreadId
finishThread (StopThread tid ThreadFinished) = Just tid
finishThread _                               = Nothing

initialEventAnalysis :: EventAnalysis
initialEventAnalysis = EventAnalysis {
    _events      = []
  , __threadInfo = Map.empty
  , _openEvents  = Map.empty
  }

{-------------------------------------------------------------------------------
  Computing totals
-------------------------------------------------------------------------------}

-- | Total amount of time spent per event
computeTotals :: EventAnalysis -> Map EventId Timestamp
computeTotals EventAnalysis{..} = go Map.empty _events
  where
    go :: Map EventId Timestamp
       -> [(EventId, Timestamp, Timestamp)]
       -> Map EventId Timestamp
    go !acc [] = acc
    go !acc ((eid, start, stop) : es) =
      go (Map.insertWith' (+) eid (stop - start) acc) es

{-------------------------------------------------------------------------------
  Quantization
-------------------------------------------------------------------------------}

quantize :: Int -> EventAnalysis -> Quantized
quantize numBuckets EventAnalysis{..} = Quantized {
      quantTimes      = go Map.empty _events
    , quantThreadInfo = Map.map quantizeThreadInfo __threadInfo
    }
  where
    go :: Map (EventId, Int) Double
       -> [(EventId, Timestamp, Timestamp)]
       -> Map (EventId, Int) Double
    go acc [] = acc
    go acc ((tid, start, end) : ttimes') =
      let startBucket = fromIntegral $ start `div` bucketSize
          endBucket   = fromIntegral $ end   `div` bucketSize
          updates     = Map.fromList
                      $ [ ((tid, b), delta startBucket endBucket start end b)
                        | b <- [startBucket .. endBucket]
                        ]
      in go (Map.unionWith (+) acc updates) ttimes'

    --           (a,                    b)
    --       |          |   ...    |          |
    --        startBucket             endBucket
    --
    --                      ^^^
    --                      bucket

    delta :: Int -> Int -> Timestamp -> Timestamp -> Int -> Double
    delta startBucket endBucket start end b = case () of
      () | b == startBucket && startBucket == endBucket ->
            t2d (end - start) / t2d bucketSize
         | b == startBucket ->
            let bEnd = startTime + fromIntegral (b + 1) * bucketSize
            in t2d (bEnd - start) / t2d bucketSize
         | b == endBucket ->
            let bStart = startTime + fromIntegral b * bucketSize
            in t2d (end - bStart) / t2d bucketSize
         | otherwise ->
            1

    startTime, endTime, bucketSize :: Timestamp
    startTime  = minimum $ map (\(_eid, start, _stop) -> start) _events
    endTime    = maximum $ map (\(_eid, _start, stop) -> stop)  _events
    bucketSize = (endTime - startTime) `div` fromIntegral numBuckets

    t2d :: Timestamp -> Double
    t2d = fromInteger . toInteger

    quantizeThreadInfo :: (Timestamp, Timestamp, String) -> (Int, Int, String)
    quantizeThreadInfo (start, stop, label) =
      ( fromIntegral $ start `div` bucketSize
      , fromIntegral $ stop  `div` bucketSize
      , label
      )
