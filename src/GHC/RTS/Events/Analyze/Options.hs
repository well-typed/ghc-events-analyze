{-# LANGUAGE CPP #-}
module GHC.RTS.Events.Analyze.Options (
    Options(..)
  , parseOptions
  ) where

import Data.Foldable (asum)
import Options.Applicative

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mconcat)
#endif

import GHC.RTS.Events.Analyze.Types
import GHC.RTS.Events.Analyze.Script
import GHC.RTS.Events.Analyze.Script.Standard

parseOptions :: IO Options
parseOptions = customExecParser (prefs showHelpOnError) opts
  where
    opts = info (helper <*> parserOptions) $ mconcat [
        fullDesc
      , progDesc "Quantize and visualize EVENTLOG"
      , footer "If no output is selected, generates SVG and totals."
      ]

parserOptions :: Parser Options
parserOptions =
        (infoOption scriptHelp $ mconcat [
            long "help-script"
          , help "Detailed information about scripts"
          ])
    <*> (selectDefaultOutput <$> (Options
      <$> (switch $ mconcat [
              long "timed"
            , help "Generate timed report (in SVG format)"
            ])
      <*> (switch $ mconcat [
              long "timed-txt"
            , help "Generate timed report (in textual format)"
            ])
      <*> (switch $ mconcat [
              long "totals"
            , help "Generate totals report"
            ])
      <*> (strOption $ mconcat [
              long "window"
            , metavar "NAME"
            , help "Events named NAME act to mark bounds of visualization window."
            , value ""
            ])
      <*> (option auto $ mconcat [
              long "buckets"
            , short 'b'
            , metavar "INT"
            , help "Use INT buckets for quantization."
            , showDefault
            , value 1000
            ])
      <*> (strOption $ mconcat [
              long "start"
            , metavar "STR"
            , help "Use STR as the prefix for the start of user events"
            , showDefault
            , value "START "
            ])
      <*> (strOption $ mconcat [
              long "stop"
            , metavar "STR"
            , help "Use STR as the prefix for the end of user events"
            , showDefault
            , value "STOP "
            ])
      <*> (strOption $ mconcat [
              long "script-totals"
            , metavar "PATH"
            , help "Use the script in PATH for the totals report"
            , value ""
            ])
      <*> (strOption $ mconcat [
              long "script-timed"
            , metavar "PATH"
            , help "Use the script in PATH for the timed reports"
            , value ""
            ])
      <*> parseTimelineGranularity
      <*> (option auto $ mconcat [
              long "tick-every"
            , metavar "N"
            , help "Render a tick every N buckets"
            , value 1
            , showDefault
            ])
      <*> (option auto $ mconcat [
              long "bucket-width"
            , metavar "DOUBLE"
            , help "Width of every bucket"
            , value 14
            , showDefault
            ])
      <*> (option auto $ mconcat [
              long "bucket-height"
            , metavar "DOUBLE"
            , help "Height of every bucket"
            , value 14
            , showDefault
            ])
      <*> (option auto $ mconcat [
              long "border-width"
            , metavar "DOUBLE"
            , help "Width of the border around each bucket (set to 0 for none)"
            , value 0.1
            , showDefault
            ])
      <*> argument str (metavar "EVENTLOG")
    ))

parseTimelineGranularity :: Parser TimelineGranularity
parseTimelineGranularity = asum [
      flag' TimelineMilliseconds $ mconcat [
          long "ms"
        , help "Use milliseconds (rather than seconds) on SVG timeline"
        ]
    , pure TimelineSeconds
    ]

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
                   , optionsGenerateTimedText  = True
                   }
      else options
  where
    noOutputSelected = not optionsGenerateTotalsText
                    && not optionsGenerateTimedSVG
                    && not optionsGenerateTimedText
