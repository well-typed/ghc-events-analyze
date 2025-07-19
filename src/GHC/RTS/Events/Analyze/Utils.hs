{-# LANGUAGE OverloadedStrings #-}
module GHC.RTS.Events.Analyze.Utils (
    throwLeft
  , throwLeftStr
  , insertWith
  , explode
  , mapEithers
  , unsparse
  , Alignment(..)
  , renderTable
  , showThreadId
  ) where

import Control.Exception
import Control.Lens
import Data.Either (partitionEithers)
import Data.List (transpose)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.RTS.Events (ThreadId)

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
{-# INLINE unsparse #-}
unsparse :: FoldableWithIndex Int f => t -> f t -> [t]
unsparse blank = go 0 . itoList
  where
    --go :: Int -> [(Int, a)] -> [a]
    go _ []            = []
    go n ((m, a) : as) = replicate (m - n) blank ++ a : go (m + 1) as

-- | Alignment options for `renderTable`
data Alignment = AlignLeft | AlignRight

-- | "Typeset" a table
renderTable :: [Alignment] -> [[Text]] -> [[Text]]
renderTable aligns rows = transpose paddedColumns
  where
    columns :: [[Text]]
    columns = transpose rows

    columnWidths :: [Int]
    columnWidths = map (maximum . map T.length) columns

    paddedColumns :: [[Text]]
    paddedColumns = map padColumn (zip3 aligns columnWidths columns)

    padColumn :: (Alignment, Int, [Text]) -> [Text]
    padColumn (align, width, column) = map (padCell align width) column

    padCell :: Alignment -> Int -> Text -> Text
    padCell align width cell =
      let padding = T.replicate (width - T.length cell) " "
      in case align of
           AlignLeft  -> cell <> padding
           AlignRight -> padding <> cell

showThreadId :: ThreadId -> Text
showThreadId = T.pack . show
