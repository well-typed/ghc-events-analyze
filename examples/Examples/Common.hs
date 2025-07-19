module Examples.Common (
    fib
  , printFib
  , blips
  ) where

import Control.Concurrent (threadDelay)

-- Intentionally slow fib
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

printFib :: Integer -> IO ()
printFib n = print (fib n)

blips :: IO ()
blips = do
  putStrLn "BLIP"
  threadDelay 5000000
  putStrLn "BLIP"

