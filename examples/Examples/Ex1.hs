module Examples.Ex1 (main) where

import Control.Concurrent (myThreadId, threadDelay)
import Control.Concurrent.Async (Async, async, wait)
import Control.Exception (bracket_)
import Debug.Trace (traceEventIO)
import GHC.Conc (labelThread)

import Examples.Common

event :: String -> IO a -> IO a
event label =
  bracket_ (traceEventIO $ "START " ++ label)
           (traceEventIO $ "STOP "  ++ label)

async' :: String -> IO a -> IO (Async a)
async' label act = async $ do
  tid <- myThreadId
  labelThread tid label
  act

printFib' :: Integer -> IO ()
printFib' n = event ("fib" ++ show n) $ printFib n

main :: IO ()
main = do
  -- See 'Ex0' on how to tweak the parameters to 'fib'
  a1 <- async' "evens" $ mapM_ printFib' [30, 32 .. 42]
  a2 <- async' "odds"  $ mapM_ printFib' [31, 33 .. 43]
  threadDelay 5000000
  a3 <- async' "blips"  $ blips
  mapM_ wait [a1, a2, a3]
