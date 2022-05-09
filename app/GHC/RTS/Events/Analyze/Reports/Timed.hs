{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module GHC.RTS.Events.Analyze.Reports.Timed (
    Report
  , ReportFragment(..)
  , ReportLine(..)
  , createReport
  , writeReport
  ) where

import Control.Lens (itoList, (^.), over, each, _3)
import Data.Function (on)
import Data.List (group, sortBy)
import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO (Handle, withFile, IOMode(WriteMode))
import Text.Printf (printf)
import qualified Data.HashMap.Strict as Map
import qualified Data.IntMap.Strict as IntMap

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
  deriving Show

data ReportLine = ReportLineData {
    lineHeader     :: Text
  , lineEventIds   :: [EventId]
  , lineBackground :: Maybe (Int, Int)
  , lineValues     :: IntMap Double
  }
  deriving Show

{-------------------------------------------------------------------------------
  Report generation
-------------------------------------------------------------------------------}

createReport :: EventAnalysis -> Quantized -> Script String -> Report
createReport analysis Quantized{..} = concatMap go . fmap (fmap (mkThreadFilter (analysis^.windowThreadInfo)))
  where
    go :: Command (ThreadId -> Bool)-> [ReportFragment]
    go (Section title) =
      [ReportSection title]
    go (One eid title) =
      [ReportLine $ reportLine title (eid, quantTimesForEvent eid)]
    go (All f sort) =
      map (ReportLine . reportLine Nothing) (sorted sort $ filtered f)
    go (Sum f title) =
      [ReportLine $ sumLines title $ map (reportLine Nothing) (filtered f)]

    quantThreadInfoFlattened = over (each._3) flattenThreadLabels quantThreadInfo
    flattenThreadLabels = T.intercalate ":" . map head . group

    reportLine :: Maybe Title -> (EventId, IntMap Double) -> ReportLine
    reportLine title (eid, qs) = ReportLineData {
        lineHeader     = showTitle (showEventId quantThreadInfoFlattened eid) title
      , lineEventIds   = [eid]
      , lineBackground = background eid
      , lineValues     = qs
      }

    -- For threads we draw a background showing the thread's lifetime
    background :: EventId -> Maybe (Int, Int)
    background EventGC           = Nothing
    background (EventUser _ _)   = Nothing
    background (EventThread tid) =
      case Map.lookup tid quantThreadInfo of
        Just (start, stop, _) -> Just (start, stop)
        Nothing               -> error $ "Invalid thread ID " ++ show tid

    quantTimesForEvent :: EventId -> IntMap Double
    quantTimesForEvent eid =
      case Map.lookup eid quantTimes of
        Nothing    -> mempty -- this event didn't happen in the window
        Just times -> times

    sorted :: Maybe EventSort -> [(EventId, a)] -> [(EventId, a)]
    sorted Nothing     = id
    sorted (Just sort) = sortBy (compareEventIds analysis sort `on` fst)

    filtered :: EventFilter (ThreadId -> Bool) -> [(EventId, IntMap Double)]
    filtered f = filter (matchesFilter f . fst) (itoList quantTimes)

sumLines :: Maybe Title -> [ReportLine] -> ReportLine
sumLines title qs = ReportLineData {
      lineHeader     = showTitle "TOTAL" title
    , lineEventIds   = concatMap lineEventIds qs
    , lineBackground = foldr1 combineBG $ map lineBackground qs
    , lineValues     = IntMap.unionsWith (+) $ map lineValues qs
    }
  where
    combineBG :: Maybe (Int, Int) -> Maybe (Int, Int) -> Maybe (Int, Int)
    combineBG (Just (fr, to)) (Just (fr', to')) = Just (min fr fr', max to to')
    combineBG _ _ = Nothing

showTitle :: a -> Maybe a -> a
showTitle _   (Just title) = title
showTitle def Nothing      = def

{-------------------------------------------------------------------------------
  Write the report in textual form
-------------------------------------------------------------------------------}

writeReport :: Report -> FilePath -> IO ()
writeReport report path = withFile path WriteMode $ writeReport' report

writeReport' :: Report -> Handle -> IO ()
writeReport' report h =
      mapM_ writeLine
    $ mapEithers id (renderTable (AlignLeft : repeat AlignRight))
    $ map reportFragment report
  where
    writeLine :: Either Text [Text] -> IO ()
    writeLine (Left header) = T.hPutStrLn h $ "\n" <> header
    writeLine (Right cells) = T.hPutStrLn h $ T.intercalate " " cells

    reportFragment :: ReportFragment -> Either Text [Text]
    reportFragment (ReportSection title) = Left title
    reportFragment (ReportLine line)     = Right (reportLine line)

    reportLine :: ReportLine -> [Text]
    reportLine ReportLineData{..} =
      lineHeader : unsparse "0.00" (over each showValue lineValues)

    showValue :: Double -> Text
    showValue = T.pack . printf "%0.2f"
