module GHC.RTS.Events.Analyze.Options (
    Options(..)
  , parseOptions
  ) where

import Options.Applicative

import GHC.RTS.Events.Analyze.Types
import GHC.RTS.Events.Analyze.Script
import GHC.RTS.Events.Analyze.Script.Standard

parseOptions :: IO Options
parseOptions = customExecParser (prefs showHelpOnError) opts
  where
    opts = info (helper <*> parserOptions)
              ( fullDesc
             <> progDesc "Quantize and visualize EVENTLOG"
             <> footer "If no output is selected, generates SVG and totals."
              )

parserOptions :: Parser Options
parserOptions =
      infoOption scriptHelp ( long "help-script"
                           <> help "Detailed information about scripts"
                            )
  <*> (selectDefaultOutput <$> (Options
    <$> switch     ( long "timed"
                  <> help "Generate timed report (in SVG format)"
                   )
    <*> switch     ( long "timed-txt"
                  <> help "Generate timed report (in textual format)"
                   )
    <*> switch     ( long "totals"
                  <> help "Generate totals report"
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
    <*> strOption  ( long "script-timed"
                  <> metavar "PATH"
                  <> help "Use the script in PATH for the timed reports"
                  <> value ""
                   )
    <*> argument str (metavar "EVENTLOG")
  ))

scriptHelp :: String
scriptHelp = unlines [
      "Scripts are used to drive report generation. The syntax for scripts is"
    , ""
    , "<script>  ::= <command>+"
    , ""
    , "<command> ::= \"section\" STRING"
    , "            | <eventId>      (\"as\" STRING)?"
    , "            | \"sum\" <filter> (\"as\" STRING)?"
    , "            | <filter>       (\"by\" <sort>)?"
    , ""
    , "<eventId> ::= STRING     -- user event"
    , "            | INT        -- thread event"
    , "            | \"GC\"       -- garbage collection"
    , ""
    , "<filter>  ::= <eventId>  -- single event"
    , "            | \"user\"     -- any user event"
    , "            | \"thread\"   -- any thread event"
    , "            | \"any\" \"[\" <filter> (\",\" <filter>)* \"]\""
    , ""
    , "<sort>    ::= \"total\""
    , "            | \"name\""
    , ""
    , "The default script for the timed reports is\n"
    ]
    ++ indent (unparseScript defaultScriptTimed)
    ++ "\nThe default script for the totals report is\n\n"
    ++ indent (unparseScript defaultScriptTotals)
    ++ unlines [
      "\nCustom scripts are useful extract different kinds of data from the"
    , "eventlog, or for presentation purposes."
    ]
  where
    indent :: [String] -> String
    indent = unlines . map ("  " ++)

-- | Select which output is active if no output is selected at all
selectDefaultOutput :: Options -> Options
selectDefaultOutput options@Options{..} =
    if noOutputSelected
      then options { optionsGenerateTotalsText = True
                   , optionsGenerateTimedSVG   = True
                   }
      else options
  where
    noOutputSelected = not optionsGenerateTotalsText
                    && not optionsGenerateTimedSVG
                    && not optionsGenerateTimedText
