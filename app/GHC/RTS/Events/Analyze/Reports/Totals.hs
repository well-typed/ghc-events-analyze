{-# LANGUAGE OverloadedStrings #-}
module GHC.RTS.Events.Analyze.Reports.Totals (
    Report
  , ReportFragment(..)
  , ReportLine(..)
  , createReport
  , writeReport
  ) where

import Control.Lens hiding (filtered)
import Data.Function (on)
import Data.List (sortBy, group)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.RTS.Events (Timestamp)
import System.IO (Handle, withFile, IOMode(WriteMode))
import Text.Printf (printf)

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
  deriving Show

data ReportLine = ReportLineData {
    lineHeader   :: Text
  , lineEventIds :: [EventId]
  , lineTotal    :: Timestamp
  }
  deriving Show

{-------------------------------------------------------------------------------
  Report generation
-------------------------------------------------------------------------------}

createReport :: EventAnalysis -> Script String -> Report
createReport analysis@EventAnalysis{..} = concatMap go . fmap (fmap (mkThreadFilter _windowThreadInfo))
  where
    go :: Command (ThreadId -> Bool) -> [ReportFragment]
    go (Section title) =
      [ReportSection title]
    go (One eid title) =
      [ReportLine $ reportLine title (eid, totalForEvent eid)]
    go (All f sort) =
      map (ReportLine . reportLine Nothing) (sorted sort $ filtered f)
    go (Sum f title) =
      [ReportLine $ sumLines title $ map (reportLine Nothing) (filtered f)]

    flattenedThreadInfo = over (each._3) flattenThreadLabels _windowThreadInfo
    flattenThreadLabels = T.intercalate ":" . map head . group

    reportLine :: Maybe Title -> (EventId, Timestamp) -> ReportLine
    reportLine title (eid, total) = ReportLineData {
        lineHeader   = showTitle (showEventId flattenedThreadInfo eid) title
      , lineEventIds = [eid]
      , lineTotal    = total
      }

    totalForEvent :: EventId -> Timestamp
    totalForEvent = eventTotal analysis

    sorted :: Maybe EventSort -> [(EventId, a)] -> [(EventId, a)]
    sorted Nothing     = id
    sorted (Just sort) = sortBy (compareEventIds analysis sort `on` fst)

    filtered :: EventFilter (ThreadId -> Bool) -> [(EventId, Timestamp)]
    filtered f = filter (matchesFilter f . fst) (itoList eventTotals)

sumLines :: Maybe Title -> [ReportLine] -> ReportLine
sumLines title qs = ReportLineData {
    lineHeader   = showTitle "TOTAL" title
  , lineEventIds = concatMap lineEventIds qs
  , lineTotal    = foldr (+) 0 $ map lineTotal qs
  }

showTitle :: Text -> Maybe Title -> Text
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
    writeLine :: Either Title [Text] -> IO ()
    writeLine (Left header) = T.hPutStrLn h $ "\n" <> header
    writeLine (Right cells) = T.hPutStrLn h $ T.intercalate "   " cells

    reportFragment :: ReportFragment -> Either Title [Text]
    reportFragment (ReportSection title) = Left title
    reportFragment (ReportLine line)     = Right (reportLine line)

    reportLine :: ReportLine -> [Text]
    reportLine ReportLineData{..} =
      [ lineHeader
      , T.pack $ printf "%dns"   $ lineTotal
      , T.pack $ printf "%0.3fs" $ toSec lineTotal
      ]

    toSec :: Timestamp -> Double
    toSec = (/ 1000000000) . fromInteger . toInteger
