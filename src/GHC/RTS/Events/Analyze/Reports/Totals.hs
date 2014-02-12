module GHC.RTS.Events.Analyze.Reports.Totals (
    Report
  , ReportFragment(..)
  , ReportLine(..)
  , createReport
  , writeReport
  ) where

import Data.Function (on)
import Data.List (sortBy, intercalate)
import GHC.RTS.Events (Timestamp)
import System.IO (Handle, hPutStrLn, withFile, IOMode(WriteMode))
import Text.Printf (printf)
import qualified Data.Map as Map

import GHC.RTS.Events.Analyze.Analysis
import GHC.RTS.Events.Analyze.Types
import GHC.RTS.Events.Analyze.Script
import GHC.RTS.Events.Analyze.Utils

{-------------------------------------------------------------------------------
  Types
-------------------------------------------------------------------------------}

type Report = [ReportFragment]

data ReportFragment =
    ReportSection Title
  | ReportLine ReportLine

data ReportLine = ReportLineData {
    lineHeader   :: String
  , lineEventIds :: [EventId]
  , lineTotal    :: Timestamp
  }

{-------------------------------------------------------------------------------
  Report generation
-------------------------------------------------------------------------------}

createReport :: EventAnalysis -> Script -> Report
createReport analysis@EventAnalysis{..} = concatMap go
  where
    go :: Command -> [ReportFragment]
    go (Section title) =
      [ReportSection title]
    go (One eid title) =
      [ReportLine $ reportLine title (eid, totalForEvent eid)]
    go (All f sort) =
      map (ReportLine . reportLine Nothing) (sorted sort $ filtered f)
    go (Sum f title) =
      [ReportLine $ sumLines title $ map (reportLine Nothing) (filtered f)]

    reportLine :: Maybe Title -> (EventId, Timestamp) -> ReportLine
    reportLine title (eid, total) = ReportLineData {
        lineHeader   = showTitle (showEventId __threadInfo eid) title
      , lineEventIds = [eid]
      , lineTotal    = total
      }

    totalForEvent :: EventId -> Timestamp
    totalForEvent = eventTotal analysis

    sorted :: Maybe EventSort -> [(EventId, a)] -> [(EventId, a)]
    sorted Nothing     = id
    sorted (Just sort) = sortBy (compareEventIds analysis sort `on` fst)

    filtered :: EventFilter -> [(EventId, Timestamp)]
    filtered f = filter (matchesFilter f . fst) (Map.toList eventTotals)

sumLines :: Maybe Title -> [ReportLine] -> ReportLine
sumLines title qs = ReportLineData {
    lineHeader   = showTitle "TOTAL" title
  , lineEventIds = concatMap lineEventIds qs
  , lineTotal    = foldr (+) 0 $ map lineTotal qs
  }

showTitle :: String -> Maybe Title -> String
showTitle _   (Just title) = title
showTitle def Nothing      = def

{-------------------------------------------------------------------------------
  Write report in textual form
-------------------------------------------------------------------------------}

writeReport :: Report -> FilePath -> IO ()
writeReport report path = withFile path WriteMode $ writeReport' report

writeReport' :: Report -> Handle -> IO ()
writeReport' report h =
      mapM_ writeLine
    $ mapEithers id (renderTable (AlignLeft : repeat AlignRight))
    $ map reportFragment report
  where
    writeLine :: Either String [String] -> IO ()
    writeLine (Left header) = hPutStrLn h $ "\n" ++ header
    writeLine (Right cells) = hPutStrLn h $ intercalate "   " cells

    reportFragment :: ReportFragment -> Either String [String]
    reportFragment (ReportSection title) = Left title
    reportFragment (ReportLine line)     = Right (reportLine line)

    reportLine :: ReportLine -> [String]
    reportLine ReportLineData{..} =
      [ lineHeader
      , printf "%dns"   $ lineTotal
      , printf "%0.3fs" $ toSec lineTotal
      ]

    toSec :: Timestamp -> Double
    toSec = (/ 1000000000) . fromInteger . toInteger
