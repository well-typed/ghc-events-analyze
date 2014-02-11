module GHC.RTS.Events.Analyze.Reports.Timed (
    Report
  , ReportFragment(..)
  , ReportLine(..)
  , createReport
  , writeReport
  ) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import System.IO (Handle, hPutStrLn)
import Text.Printf (printf)
import qualified Data.Map as Map

import GHC.RTS.Events.Analyze.Analysis
import GHC.RTS.Events.Analyze.Script
import GHC.RTS.Events.Analyze.Types
import GHC.RTS.Events.Analyze.Utils

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

type Report = [ReportFragment]

data ReportFragment =
    ReportSection Title
  | ReportLine ReportLine

data ReportLine = ReportLineData {
    lineHeader     :: String
  , lineEventIds   :: [EventId]
  , lineBackground :: Maybe (Int, Int)
  , lineValues     :: Map Int Double
  }

{-------------------------------------------------------------------------------
  Report generation
-------------------------------------------------------------------------------}

createReport :: EventAnalysis -> Quantized -> Script -> [ReportFragment]
createReport analysis Quantized{..} = concatMap go
  where
    go :: Command -> [ReportFragment]
    go (Section title) =
      [ReportSection title]
    go (One eid title) =
      [ReportLine $ reportLine title (eid, quantTimesForEvent eid)]
    go (All f sort) =
      map (ReportLine . reportLine Nothing) (sorted sort $ filtered f)
    go (Sum f title) =
      [ReportLine $ sumLines title $ map (reportLine Nothing) (filtered f)]

    reportLine :: Maybe Title -> (EventId, Map Int Double) -> ReportLine
    reportLine title (eid, qs) = ReportLineData {
        lineHeader     = showTitle (showEventId quantThreadInfo eid) title
      , lineEventIds   = [eid]
      , lineBackground = background eid
      , lineValues     = qs
      }

    -- For threads we draw a background showing the thread's lifetime
    background :: EventId -> Maybe (Int, Int)
    background EventGC           = Nothing
    background (EventUser _)     = Nothing
    background (EventThread tid) =
      case Map.lookup tid quantThreadInfo of
        Just (start, stop, _) -> Just (start, stop)
        Nothing               -> error $ "Invalid thread ID " ++ show tid

    quantTimesForEvent :: EventId -> Map Int Double
    quantTimesForEvent eid =
      case Map.lookup eid quantTimes of
        Nothing    -> error $ "Invalid event ID " ++ show eid ++ ". "
                           ++ "Valid IDs are " ++ show (Map.keys quantTimes)
        Just times -> times

    sorted :: Maybe EventSort -> [(EventId, a)] -> [(EventId, a)]
    sorted Nothing     = id
    sorted (Just sort) = sortBy (compareEventIds analysis sort `on` fst)

    filtered :: EventFilter -> [(EventId, Map Int Double)]
    filtered f = filter (matchesFilter f . fst) (Map.toList quantTimes)

sumLines :: Maybe Title -> [ReportLine] -> ReportLine
sumLines title qs = ReportLineData {
      lineHeader     = showTitle "TOTAL" title
    , lineEventIds   = concatMap lineEventIds qs
    , lineBackground = foldr1 combineBG $ map lineBackground qs
    , lineValues     = Map.unionsWith (+) $ map lineValues qs
    }
  where
    combineBG :: Maybe (Int, Int) -> Maybe (Int, Int) -> Maybe (Int, Int)
    combineBG (Just (fr, to)) (Just (fr', to')) = Just (min fr fr', max to to')
    combineBG _ _ = Nothing

showTitle :: String -> Maybe Title -> String
showTitle _   (Just title) = title
showTitle def Nothing      = def

{-------------------------------------------------------------------------------
  Write the report in textual form
-------------------------------------------------------------------------------}

writeReport :: Handle -> Report -> IO ()
writeReport h report = mapM_ writeFragment report
  where
    writeFragment :: ReportFragment -> IO ()
    writeFragment (ReportSection title) = hPutStrLn h $ "\n" ++ title
    writeFragment (ReportLine line)     = hPutStrLn h $ renderLine line

    renderLine :: ReportLine -> String
    renderLine ReportLineData{..} =
      applyAll (map showValue $ Map.toList lineValues) lineHeader

    showValue :: (Int, Double) -> String -> String
    showValue (b, q) = overwrite (headerWidth + 1 + b * 6) (printf "%0.2f" q)

    headerWidth :: Int
    headerWidth = maximum . catMaybes . map aux $ report
      where
        aux :: ReportFragment -> Maybe Int
        aux (ReportSection _) = Nothing
        aux (ReportLine line) = Just . length . lineHeader $ line
