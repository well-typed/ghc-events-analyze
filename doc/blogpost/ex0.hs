import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)

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

main :: IO ()
main = do
  a1 <- async $ mapM_ printFib [30, 32 .. 38]
  a2 <- async $ mapM_ printFib [31, 33 .. 39]
  threadDelay 5000000
  a3 <- async $ blips
  mapM_ wait [a1, a2, a3]
