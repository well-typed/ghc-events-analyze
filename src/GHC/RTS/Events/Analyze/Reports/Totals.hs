module GHC.RTS.Events.Analyze.Reports.Totals (
    reportTotals
  ) where

import Control.Arrow (first)
import Data.Function (on)
import Data.List (sortBy)
import GHC.RTS.Events (Timestamp)
import System.IO (Handle, hPutStrLn)
import qualified Data.Map as Map

import GHC.RTS.Events.Analyze.Analysis
import GHC.RTS.Events.Analyze.Types
import GHC.RTS.Events.Analyze.Script

reportTotals :: Handle -> EventAnalysis -> Script -> IO ()
reportTotals h analysis@EventAnalysis{..} = mapM_ go
  where
    go :: Command -> IO ()
    go (Section title) =
      hPutStrLn h $ "\n" ++ title
    go (One eid title) = do
      let t = eventTotal analysis eid
      putEventTotal (showTitle (showEventId __threadInfo eid) title, t)
    go (All f sort) =
      mapM_ putEventTotal $ map (first $ showEventId __threadInfo)
                          . sorted sort
                          $ filtered f
    go (Sum f title) = do
      let t = sum $ map snd $ filtered f
      putEventTotal (showTitle "TOTAL" title, t)

    putEventTotal :: (String, Timestamp) -> IO ()
    putEventTotal (title, total) = do
      hPutStrLn h $ title ++ ": " ++ showTotal total

    sorted :: Maybe EventSort -> [(EventId, a)] -> [(EventId, a)]
    sorted Nothing     = id
    sorted (Just sort) = sortBy (compareEventIds analysis sort `on` fst)

    filtered :: EventFilter -> [(EventId, Timestamp)]
    filtered f = filter (matchesFilter f . fst) (Map.toList eventTotals)

showTitle :: String -> Maybe Title -> String
showTitle _   (Just title) = title
showTitle def Nothing      = def

showTotal :: Timestamp -> String
showTotal t = show t ++ "ns (" ++ show (toSec t) ++ "s)"

toSec :: Timestamp -> Double
toSec = (/ 1000000000) . fromInteger . toInteger
