{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}
-- | Helper functions for working with arrays.

module Data.ART.Internal.Array where

import           Data.Array.IArray           (IArray, (!))
import           Data.Array.Unboxed          (UArray)
import           Data.Word                   (Word8)

import           Data.Vector                 (Vector, (//))
import qualified Data.Vector                 as V
import           Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MV

              -- TODO: Switch to unsafeIndex?
findIndex :: Word8 -> Word8 -> UArray Word8 Word8 -> Maybe Word8
findIndex size target arr = go 0
  where go !n | n < size  = if arr ! n == target then Just n else go (n + 1)
              | otherwise = Nothing

-- | Given a sorted array, returns the index containing the given element, if
-- any. With multiple equal elements, the index returned is
-- unspecified.
binarySearch :: Word8 -> Word8 -> UArray Word8 Word8 -> Maybe Word8
binarySearch size target arr = go 0 size
  where go !from !to = 
          let i = from + (to - from) `div` 2 in
          if | i + 1 == from || i == to -> Nothing
             | arr ! i == target        -> Just i
             | arr ! i < target         -> go (i + 1) to
             | otherwise                -> go from i

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
