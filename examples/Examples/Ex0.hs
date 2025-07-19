module Examples.Ex0 (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)

import Examples.Common

main :: IO ()
main = do
  -- To generate a picture similar to the one in the blog post, the entire test
  -- should take roughly 30 seconds. Tweak arguments to 'fib' accordingly.
  a1 <- async $ mapM_ printFib [30, 32 .. 42]
  a2 <- async $ mapM_ printFib [31, 33 .. 43]
  threadDelay 5000000
  a3 <- async $ blips
  mapM_ wait [a1, a2, a3]
