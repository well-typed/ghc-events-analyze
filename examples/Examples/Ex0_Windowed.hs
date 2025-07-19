module Examples.Ex0_Windowed (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Debug.Trace (traceEventIO)

import Examples.Common

main :: IO ()
main = do
  -- See 'Ex0' on how to tweak the parameters to 'fib'

  traceEventIO "START WINDOW"
  do a1 <- async $ mapM_ printFib [30, 32 .. 42]
     a2 <- async $ mapM_ printFib [31, 33 .. 43]
     threadDelay 5000000
     ax <- async $ blips
     mapM_ wait [a1, a2, ax]
  traceEventIO "STOP WINDOW"

  traceEventIO "START WINDOW"
  do a1 <- async $ mapM_ printFib [29, 32 .. 41]
     a2 <- async $ mapM_ printFib [30, 33 .. 42]
     a3 <- async $ mapM_ printFib [31, 34 .. 43]
     threadDelay 5000000
     ax <- async $ blips
     mapM_ wait [a1, a2, a3, ax]
  traceEventIO "STOP WINDOW"

  traceEventIO "START WINDOW"
  do a1 <- async $ mapM_ printFib [28, 32 .. 40]
     a2 <- async $ mapM_ printFib [29, 33 .. 41]
     a3 <- async $ mapM_ printFib [30, 34 .. 42]
     a4 <- async $ mapM_ printFib [31, 35 .. 43]
     threadDelay 5000000
     ax <- async $ blips
     mapM_ wait [a1, a2, a3, a4, ax]
  traceEventIO "STOP WINDOW"
