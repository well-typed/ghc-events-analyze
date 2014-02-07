module GHC.RTS.Events.Analyze.Utils (
    throwLeft
  , throwLeftStr
  , insertWith
  , prefix
  , explode
  ) where

import Control.Exception

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
