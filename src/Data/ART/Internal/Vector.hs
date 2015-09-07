{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}
-- | Some efficient helper functions for working with vectors.

module Data.ART.Internal.Vector where

import           Data.Vector                 (Vector, (!))
import qualified Data.Vector                 as V
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MV

-- | Given a sorted vector, returns an index containing the given element, if
-- any. With multiple equal elements, the index returned is
-- unspecified.
binarySearch :: Ord a => a -> Vector a -> Maybe Int
binarySearch needle haystack = go 0 (V.length haystack)
  where go !from !to =
          let i = from + (to - from) `div` 2
              k = haystack ! i
          in
          if | i + 1 == from || i == to -> Nothing
             | k == needle            -> Just i
             | k < needle            -> go (i + 1) to
             | otherwise             -> go from i
{-# INLINE binarySearch #-}

  -- TODO: Optimize?
insert :: Ord a => a -> Vector a -> Vector a
insert x vector = V.concat [before, V.singleton x, after]
  where (before, after) = V.span (< x) vector
