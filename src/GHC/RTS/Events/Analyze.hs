{-# LANGUAGE CPP #-}
module Main where

import Control.Monad (when, forM_)
import Data.Maybe (isNothing)
import System.FilePath (replaceExtension, takeFileName)
import Text.Parsec.String (parseFromFile)
import Text.Regex.PCRE

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import GHC.RTS.Events.Analyze.Analysis
import GHC.RTS.Events.Analyze.Options
import qualified GHC.RTS.Events.Analyze.Reports.Totals    as Totals
import qualified GHC.RTS.Events.Analyze.Reports.Timed     as Timed
import qualified GHC.RTS.Events.Analyze.Reports.Timed.SVG as TimedSVG
import GHC.RTS.Events.Analyze.Script
import GHC.RTS.Events.Analyze.Script.Standard

main :: IO ()
main = do
    options@Options{..} <- parseOptions
    analyses            <- analyze options <$> readEventLog optionsInput

    (timedScriptName,  timedScript)  <- getScript optionsScriptTimed  defaultScriptTimed
    (totalsScriptName, totalsScript) <- getScript optionsScriptTotals defaultScriptTotals

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

        prefixAnalysisNumber :: Int -> String -> String
        prefixAnalysisNumber i filename
          | isNothing optionsWindowEvent = filename
          | otherwise                    = show i ++ "." ++ filename

    forM_ (zip [0..] analyses) $ \ (i,analysis) -> do

      let quantized = quantize optionsNumBuckets analysis
          totals    = Totals.createReport analysis (fmap makeRegex <$> totalsScript)
          timed     = Timed.createReport analysis quantized (fmap makeRegex <$> timedScript)

      writeReport optionsGenerateTotalsText
                  totalsScriptName
                  (prefixAnalysisNumber i "totals.txt")
                  (Totals.writeReport totals)

      writeReport optionsGenerateTimedSVG
                  timedScriptName
                  (prefixAnalysisNumber i "timed.svg")
                  (TimedSVG.writeReport options quantized timed)

      writeReport optionsGenerateTimedText
                  timedScriptName
                  (prefixAnalysisNumber i "timed.txt")
                  (Timed.writeReport timed)

getScript :: FilePath -> Script String -> IO (String, Script String)
getScript ""   def = return ("default script", def)
getScript path _   = do
  mScript <- parseFromFile pScript path
  case mScript of
    Left  err    -> fail (show err)
    Right script -> return (path, script)
