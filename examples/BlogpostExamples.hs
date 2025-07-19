module BlogpostExamples (main) where

import System.Environment

import Examples.Ex0          qualified as Ex0
import Examples.Ex0_Windowed qualified as Ex0_Windowed
import Examples.Ex1          qualified as Ex1

main :: IO ()
main = do
    args <- getArgs
    case args of
      []               -> return () -- For use by @cabal test@
      ["ex0"]          -> Ex0.main
      ["ex0-windowed"] -> Ex0_Windowed.main
      ["ex1"]          -> Ex1.main
      _otherwise       -> error $ "Invalid arguments " ++ show args