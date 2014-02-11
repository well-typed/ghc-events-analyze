{-# LANGUAGE QuasiQuotes #-}
module GHC.RTS.Events.Analyze.Script.Standard (
    defaultScriptTotals
  , defaultScriptSVG
  ) where

import GHC.RTS.Events.Analyze.Script

defaultScriptTotals :: Script
defaultScriptTotals = [scriptQQ|
    GC

    section "USER EVENTS (user events are corrected for GC)"
    user by total
    sum user

    section "THREAD EVENTS"
    thread by name
    sum thread
  |]

defaultScriptSVG :: Script
defaultScriptSVG = [scriptQQ|
    GC

    section "USER EVENTS"
    user by name

    section "THREAD EVENTS"
    thread by name
  |]
