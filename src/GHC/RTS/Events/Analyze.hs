module Main where

import Control.Applicative ((<$>))
import Control.Monad (when)
import System.FilePath (replaceExtension, takeFileName)
import System.IO (withFile, IOMode(WriteMode))
import Text.Parsec.String (parseFromFile)

import GHC.RTS.Events.Analyze.Analysis
import GHC.RTS.Events.Analyze.Options
import GHC.RTS.Events.Analyze.Reports.SVG
import GHC.RTS.Events.Analyze.Reports.Totals
import GHC.RTS.Events.Analyze.Script
import GHC.RTS.Events.Analyze.Script.Standard

main :: IO ()
main = do
    options@Options{..} <- parseOptions
    analysis            <- analyze options <$> readEventLog optionsInput

    let quantized = quantize optionsNumBuckets analysis

    let writeReport :: Bool
                    -> FilePath
                    -> Script
                    -> String
                    -> (FilePath -> Script -> IO ())
                    -> IO ()
        writeReport isEnabled
                    scriptPath
                    defaultScript
                    newExt
                    mkReport = when isEnabled $ do
          let output = replaceExtension (takeFileName optionsInput) newExt
          (scriptName, script) <- getScript scriptPath defaultScript
          mkReport output script
          putStrLn $ "Generated " ++ output ++ " using " ++ scriptName

    writeReport optionsGenerateTotals
                optionsScriptTotals
                defaultScriptTotals
                "totals" $ \output script ->
      withFile output WriteMode $ \h -> reportTotals h analysis script

    writeReport optionsGenerateSVG
                optionsScriptSVG
                defaultScriptSVG
                "svg" $ \output script ->
      uncurry (renderSVG output) $ reportSVG analysis quantized script

getScript :: FilePath -> Script -> IO (String, Script)
getScript ""   def = return ("default script", def)
getScript path _   = do
  mScript <- parseFromFile pScript path
  case mScript of
    Left  err    -> fail (show err)
    Right script -> return (path, script)
