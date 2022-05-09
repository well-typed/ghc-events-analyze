{-# LANGUAGE QuasiQuotes #-}
module GHC.RTS.Events.Analyze.Script.Standard (
    defaultScriptTotals
  , defaultScriptTimed
  ) where

import GHC.RTS.Events.Analyze.Script

defaultScriptTotals :: Script String
defaultScriptTotals = [scriptQQ|
    GC

    section "USER EVENTS (user events are corrected for GC)"
    all user by total
    sum user

    section "THREAD EVENTS"
    all thread by name
    sum thread
  |]

defaultScriptTimed :: Script String
defaultScriptTimed = [scriptQQ|
    GC

    section "USER EVENTS"
    all user by name

    section "THREAD EVENTS"
    all thread by name
  |]
