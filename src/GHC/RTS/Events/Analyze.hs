module Main where

import Prelude hiding (id)
import Control.Applicative
import Control.Monad (when)
import Data.Function (on)
import Data.List (sortBy)
import GHC.RTS.Events (Timestamp)
import qualified Data.Map as Map
import System.IO (withFile, hPutStrLn, IOMode(WriteMode), Handle)
import System.FilePath (replaceExtension, takeFileName)

import GHC.RTS.Events.Analyze.Types
import GHC.RTS.Events.Analyze.Options
import GHC.RTS.Events.Analyze.Analysis
import GHC.RTS.Events.Analyze.Diagrams

main :: IO ()
main = do
    options@Options{..} <- parseOptions
    analysis            <- analyze options <$> readEventLog optionsInput

    when optionsGenerateTotals $ do
      let output = replaceExtension (takeFileName optionsInput) "totals"
      withFile output WriteMode $ printTotals options analysis
      putStrLn $ "Wrote " ++ output

    when optionsGenerateSVG $ do
      let quantized = quantize optionsNumBuckets analysis
          output    = replaceExtension (takeFileName optionsInput) "svg"
      uncurry (renderSVG output) $ diagramQuantTimes options quantized
      putStrLn $ "Wrote " ++ output

printTotals :: Options -> EventAnalysis -> Handle -> IO ()
printTotals options analysis@EventAnalysis{..} h = do
    let (gc, userEvents, threadEvents) = partitioned

    hPutStrLn h $ "TOTAL GC: " ++ showTotal gc
    hPutStrLn h ""

    hPutStrLn h $ "USER EVENTS (user events are corrected for GC)"
    mapM_ showEventTotal (reverse (sortBy (compare `on` snd) userEvents))
    hPutStrLn h $ "TOTAL: " ++ showTotal (sum (map snd userEvents))
    hPutStrLn h ""

    hPutStrLn h $ "THREADS"
    mapM_ showEventTotal (sortBy (compare `on` fst) threadEvents)
    hPutStrLn h $ "TOTAL: " ++ showTotal (sum (map snd threadEvents))
  where
    showEventTotal :: (EventId, Timestamp) -> IO ()
    showEventTotal (eid, total) =
      hPutStrLn h $ showEventId options __threadInfo eid ++ ": "
                 ++ showTotal total

    showTotal :: Timestamp -> String
    showTotal total = show total ++ "ns (" ++ show (toSec total) ++ "s)"

    toSec :: Timestamp -> Double
    toSec = (/ 1000000000) . fromInteger . toInteger

    partitioned :: ( Timestamp               -- GC
                   , [(EventId, Timestamp)]  -- User events
                   , [(EventId, Timestamp)]  -- Threads
                   )
    partitioned = go 0 [] [] (Map.toList (computeTotals analysis))
      where
        go accG accU accT []     = (accG, accU, accT)
        go accG accU accT (e:es) = case e of
          (EventGC,       time) -> go time      accU       accT  es
          (EventUser _,   _)    -> go accG (e : accU)      accT  es
          (EventThread _, _)    -> go accG      accU  (e : accT) es
