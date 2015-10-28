{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Helper functions for working with arrays.

module Data.ART.Internal.Array where

import           Control.Monad               (when)
import           Control.Monad.ST            (runST, ST)

import           Data.Array.IArray           (Array, IArray, (!), Ix)
import qualified Data.Array.IArray           as Array
import           Data.Array.MArray           (MArray)
import qualified Data.Array.MArray           as MArray
import           Data.Array.ST               (STArray, STUArray)
import           Data.Array.Unsafe           (unsafeFreeze)
import           Data.Array.Unboxed          (UArray)
import           Data.Word                   (Word8)

type Key = Word8
type Keys = UArray Key Key
type Values a = Array Word8 a

              -- TODO: Switch to unsafeIndex?
findIndex :: Key -> Keys -> Maybe Key
findIndex target arr = go 0
  where (_, size) = Array.bounds arr
        go !n | n < size  = if arr ! n == target then Just n else go (n + 1)
              | otherwise = Nothing

-- | Given a sorted array, returns the index containing the given element, if
-- any. With multiple equal elements, the index returned is
-- unspecified.
binarySearch :: Key -> Keys -> Maybe Key
binarySearch target arr = go 0 size
  where (_, size) = Array.bounds arr
        go !from !to =
          let i = from + (to - from) `div` 2 in
          if | i + 1 == from || i == to -> Nothing
             | arr ! i == target        -> Just i
             | arr ! i < target         -> go (i + 1) to
             | otherwise                -> go from i

          -- TODO: Man, this code is ugly!
-- | Given a key, a value, a sorted array of keys and an array of
-- values, inserts the key into the sorted array and inserts the value
-- into the corresponding index of the value array. *Should* only copy
-- each array once.
--
-- The array has to be 0-indexed!
--
-- If the key is already in the keys array some number of times, the
-- new key will be inserted after all the existing ones.
insert :: forall a. Key -> a -> Keys -> Values a -> (Keys, Values a)
insert key value keys values = runST newArrays
  where size = snd (Array.bounds keys) + 1

        newArrays :: forall s. ST s (Keys, Values a)
        newArrays =
          do -- Remember: Array bounds are *inclusive*!
             newKeys <- MArray.newArray (0, size) 0 -- initialized to 0 because unboxed
             newValues <- MArray.newArray (0, size) (error "uninitialized")
             go newKeys newValues 0
             keys <- unsafeFreeze newKeys
             values <- unsafeFreeze newValues
             return (keys, values)

        go :: forall s. STUArray s Key Key -> STArray s Key a -> Key -> ST s ()
        go newKeys newValues = go' False
          where go' done n
                  | n == size = when (not done) $ do
                      write n key value
                  | done = do
                      write (n + 1) (keys ! n) (values ! n)
                      go' True $ n + 1
                  | keys ! n <= key = do
                      write n (keys ! n) (values ! n)
                      go' False $ n + 1
                  | keys ! n > key = do
                      write n key value
                      go' True $ n
                write n k v = MArray.writeArray newKeys n k >> MArray.writeArray newValues n v

-- | Insert a key-value pair into a vector sorted by keys. If the
-- -- given key already exists, combine the old value and the new value
-- -- with the given function.
-- insertWith :: Ord a => (b -> b -> b) -> (a, b) -> Vector (a, b) -> Vector (a, b)
-- insertWith f (k, v) vector
--   | length after > 0 && fst (V.head after) == k =
--     V.concat [before, V.singleton (k, f v . snd $ V.head after), V.tail after]
--   | otherwise =
--     V.concat [before, V.singleton (k, v), after]
--   where (before, after) = V.span (\ (k', _) -> k' < k) vector
