module Main where

import Control.Applicative ((<$>))
import Control.Monad (when)
import System.FilePath (replaceExtension, takeFileName)
import System.IO (withFile, IOMode(WriteMode))
import Text.Parsec.String (parseFromFile)

import GHC.RTS.Events.Analyze.Analysis
import GHC.RTS.Events.Analyze.Options
import qualified GHC.RTS.Events.Analyze.Reports.Timed     as Timed
import qualified GHC.RTS.Events.Analyze.Reports.Timed.SVG as TimedSVG
import GHC.RTS.Events.Analyze.Reports.Totals
import GHC.RTS.Events.Analyze.Script
import GHC.RTS.Events.Analyze.Script.Standard

main :: IO ()
main = do
    options@Options{..} <- parseOptions
    analysis            <- analyze options <$> readEventLog optionsInput

    (timedScriptName,  timedScript)  <- getScript optionsScriptTimed  defaultScriptTimed
    (totalsScriptName, totalsScript) <- getScript optionsScriptTotals defaultScriptTotals

    let quantized = quantize optionsNumBuckets analysis
        timed     = Timed.createReport analysis quantized timedScript

    let writeReport :: Bool
                    -> String
                    -> String
                    -> (FilePath -> IO ())
                    -> IO ()
        writeReport isEnabled
                    scriptName
                    newExt
                    mkReport = when isEnabled $ do
          let output = replaceExtension (takeFileName optionsInput) newExt
          mkReport output
          putStrLn $ "Generated " ++ output ++ " using " ++ scriptName

    writeReport optionsGenerateTotalsText
                totalsScriptName
                "totals.txt" $ \output ->
      withFile output WriteMode $ \h -> reportTotals h analysis totalsScript

    writeReport optionsGenerateTimedSVG
                timedScriptName
                "timed.svg" $ \output ->
      TimedSVG.writeReport output timed

    writeReport optionsGenerateTimedText
                timedScriptName
                "timed.txt" $ \output ->
      withFile output WriteMode $ \h ->
        Timed.writeReport h timed

getScript :: FilePath -> Script -> IO (String, Script)
getScript ""   def = return ("default script", def)
getScript path _   = do
  mScript <- parseFromFile pScript path
  case mScript of
    Left  err    -> fail (show err)
    Right script -> return (path, script)
