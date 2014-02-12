module GHC.RTS.Events.Analyze.Utils (
    throwLeft
  , throwLeftStr
  , insertWith
  , prefix
  , explode
  , mapEithers
  , unsparse
  , Alignment(..)
  , renderTable
  ) where

import Control.Exception
import Data.List (transpose)
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map

throwLeft :: Exception e => IO (Either e a) -> IO a
throwLeft act = act >>= \ea -> case ea of Left  e -> throwIO e
                                          Right a -> return a

throwLeftStr :: IO (Either String a) -> IO a
throwLeftStr = throwLeft . fmap (either (Left . userError) Right)

-- | Like `Map.insertWith`, but for associative lists
--
-- > updateAssocs f key val [.. (key, val') ..] == [.. (key, val' `f` val) ..]
-- > updateAssocs f key val assocs == assocs ++ [(key, val)]
insertWith :: Eq a => (b -> b -> b) -> a -> b -> [(a, b)] -> [(a, b)]
insertWith f key val = go
  where
    go [] = [(key, val)]
    go ((key', val') : assocs)
      | key == key' = (key, val' `f` val) : assocs
      | otherwise   = (key', val')        : go assocs

-- | Like PHP's explode function
--
-- > explode ',' "abc,def,ghi" == ["abc","def","ghi"]
explode :: Eq a => a -> [a] -> [[a]]
explode needle = go
  where
    go xs = case break (== needle) xs of
              (before, [])        -> [before]
              (before, _ : after) -> before : go after

-- | Check if a string has a given prefix
--
-- > prefix "abc" "abcdef" == Just "def"
-- > prefix "abc" "defabc" == Nothing
prefix :: String -> String -> Maybe String
prefix []     ys                 = Just ys
prefix _      []                 = Nothing
prefix (x:xs) (y:ys) | x == y    = prefix xs ys
                     | otherwise = Nothing

mapEithers :: forall a b c d.
              ([a] -> [c])
           -> ([b] -> [d])
           -> [Either a b]
           -> [Either c d]
mapEithers f g eithers = rebuild eithers (f lefts) (g rights)
  where
    (lefts, rights) = partitionEithers eithers

    rebuild :: [Either a b] -> [c] -> [d] -> [Either c d]
    rebuild []             []       []       = []
    rebuild (Left  _ : es) (x : xs)      ys  = Left  x : rebuild es xs ys
    rebuild (Right _ : es)      xs  (y : ys) = Right y : rebuild es xs ys
    rebuild _ _ _ = error "mapEithers: lengths changed"

-- | Turn a sparse representation of a list into a regular list, using
-- a default value for the blanks
unsparse :: forall a. a -> Map Int a -> [a]
unsparse blank = go 0 . Map.toList
  where
    go :: Int -> [(Int, a)] -> [a]
    go _ []            = []
    go n ((m, a) : as) = replicate (m - n) blank ++ a : go (m + 1) as

-- | Alignment options for `renderTable`
data Alignment = AlignLeft | AlignRight

-- | "Typeset" a table
renderTable :: [Alignment] -> [[String]] -> [[String]]
renderTable aligns rows = transpose paddedColumns
  where
    columns :: [[String]]
    columns = transpose rows

    columnWidths :: [Int]
    columnWidths = map (maximum . map length) columns

    paddedColumns :: [[String]]
    paddedColumns = map padColumn (zip3 aligns columnWidths columns)

    padColumn :: (Alignment, Int, [String]) -> [String]
    padColumn (align, width, column) = map (padCell align width) column

    padCell :: Alignment -> Int -> String -> String
    padCell align width cell =
      let padding = replicate (width - length cell) ' '
      in case align of
           AlignLeft  -> cell ++ padding
           AlignRight -> padding ++ cell
