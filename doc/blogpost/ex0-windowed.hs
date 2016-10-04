import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, wait)
import Debug.Trace (traceEventIO)

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
  traceEventIO "START WINDOW"
  do a1 <- async $ mapM_ printFib [30, 32, 34, 36, 38]
     a2 <- async $ mapM_ printFib [31, 33, 35, 37, 39]
     threadDelay 5000000
     ax <- async $ blips
     mapM_ wait [a1, a2, ax]
  traceEventIO "STOP WINDOW"

  traceEventIO "START WINDOW"
  do a1 <- async $ mapM_ printFib [25, 28, 31, 34, 37]
     a2 <- async $ mapM_ printFib [26, 29, 32, 35, 38]
     a3 <- async $ mapM_ printFib [27, 30, 33, 36, 39]
     threadDelay 5000000
     ax <- async $ blips
     mapM_ wait [a1, a2, a3, ax]
  traceEventIO "STOP WINDOW"

  traceEventIO "START WINDOW"
  do a1 <- async $ mapM_ printFib [20, 24, 28, 32, 36]
     a2 <- async $ mapM_ printFib [21, 25, 29, 33, 37]
     a3 <- async $ mapM_ printFib [22, 26, 30, 34, 38]
     a4 <- async $ mapM_ printFib [23, 27, 31, 35, 39]
     threadDelay 5000000
     ax <- async $ blips
     mapM_ wait [a1, a2, a3, a4, ax]
  traceEventIO "STOP WINDOW"
