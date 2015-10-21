{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}
-- | Some efficient helper functions for working with vectors.

module Data.ART.Internal.Vector where

import           Data.Vector                 (Vector, (!), (//))
import qualified Data.Vector                 as V
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MV

-- | Given a sorted vector, returns an index containing the given element, if
-- any. With multiple equal elements, the index returned is
-- unspecified.
binarySearch :: Ord a => a -> Vector (a, b) -> Maybe (a, b)
binarySearch needle haystack = go 0 (V.length haystack)
  where go !from !to =
          let i      = from + (to - from) `div` 2
              (k, v) = haystack ! i
          in
          if | i + 1 == from || i == to -> Nothing
             | k == needle              -> Just (k, v)
             | k < needle               -> go (i + 1) to
             | otherwise                -> go from i
{-# INLINE binarySearch #-}

  -- TODO: Optimize?
-- | Insert a key-value pair into a vector sorted by keys. If the
-- given key already exists, combine the old value and the new value
-- with the given function.
insertWith :: Ord a => (b -> b -> b) -> (a, b) -> Vector (a, b) -> Vector (a, b)
insertWith f (k, v) vector
  | length after > 0 && fst (V.head after) == k =
    V.concat [before, V.singleton (k, f v . snd $ V.head after), V.tail after]
  | otherwise =
    V.concat [before, V.singleton (k, v), after]
  where (before, after) = V.span (\ (k', _) -> k' < k) vector
