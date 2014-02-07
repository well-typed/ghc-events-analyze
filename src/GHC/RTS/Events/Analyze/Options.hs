module GHC.RTS.Events.Analyze.Options (
    Options(..)
  , parseOptions
  ) where

import Options.Applicative

import GHC.RTS.Events.Analyze.Types
import GHC.RTS.Events.Analyze.Utils

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
                  <> metavar "BUCKETS"
                  <> help "Use BUCKETS buckets for quantization."
                  <> showDefault
                  <> value 100
                   )
    <*> nullOption ( long "filter"
                  <> metavar "LIST"
                  <> help "Only show the events in LIST. LIST must be a comma separated list of event IDs, where an event ID is either \"GC\", a string (for user events), or an int (for thread IDs)."
                  <> reader readEventIds
                  <> value Nothing
                   )
    <*> nullOption ( long "rename"
                  <> metavar "LIST"
                  <> help "Rename events before showing them in the diagram. LIST must be a comma separated list of ID=string fields."
                  <> reader readEventNames
                  <> value []
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

readEventIds :: String -> ReadM (Maybe [EventId])
readEventIds = fmap Just . mapM readEventId . explode ','

readEventId :: String -> ReadM EventId
readEventId "GC"                 = return EventGC
readEventId (reads -> [(i, "")]) = return (EventThread i)
readEventId e                    = return (EventUser e)

readEventNames :: String -> ReadM [(EventId, String)]
readEventNames = mapM readEventName . explode ','

readEventName :: String -> ReadM (EventId, String)
readEventName s =
  case break (== '=') s of
    (before, '=' : after) -> do
      eid <- readEventId before
      return (eid, after)
    _ ->
      fail $ "Unexpected " ++ show s ++ ". Expected ID=string"
