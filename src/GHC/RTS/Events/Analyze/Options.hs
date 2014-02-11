module GHC.RTS.Events.Analyze.Options (
    Options(..)
  , parseOptions
  ) where

import Options.Applicative

import GHC.RTS.Events.Analyze.Types

parseOptions :: IO Options
parseOptions = customExecParser (prefs showHelpOnError) opts
  where
    opts = info (helper <*> parserOptions)
              ( fullDesc
             <> progDesc "Quantize and visualize EVENTLOG"
             <> footer "If no output is selected, generates SVG and totals."
              )

parserOptions :: Parser Options
parserOptions = selectDefaultOutput <$> (Options
    <$> switch     ( long "svg"
                  <> help "Generate SVG file"
                   )
    <*> switch     ( long "totals"
                  <> help "Generate totals"
                   )
    <*> option     ( long "buckets"
                  <> short 'b'
                  <> metavar "INT"
                  <> help "Use INT buckets for quantization."
                  <> showDefault
                  <> value 100
                   )
    <*> strOption  ( long "start"
                  <> metavar "STR"
                  <> help "Use STR as the prefix for the start of user events"
                  <> showDefault
                  <> value "START "
                   )
    <*> strOption  ( long "stop"
                  <> metavar "STR"
                  <> help "Use STR as the prefix for the end of user events"
                  <> showDefault
                  <> value "STOP "
                   )
    <*> strOption  ( long "script-totals"
                  <> metavar "PATH"
                  <> help "Use the script in PATH for the totals report"
                  <> value ""
                   )
    <*> strOption  ( long "script-svg"
                  <> metavar "PATH"
                  <> help "Use the script in PATH for the SVG report"
                  <> value ""
                   )
    <*> argument str (metavar "EVENTLOG")
  )

-- | Select which output is active if no output is selected at all
selectDefaultOutput :: Options -> Options
selectDefaultOutput options@Options{..} =
    if noOutputSelected
      then options { optionsGenerateTotals = True
                   , optionsGenerateSVG    = True
                   }
      else options
  where
    noOutputSelected = not optionsGenerateTotals
                    && not optionsGenerateSVG
