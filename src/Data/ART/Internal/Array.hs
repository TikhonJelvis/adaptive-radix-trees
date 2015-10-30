{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Helper functions for working with arrays.

module Data.ART.Internal.Array where

import           Control.Monad               (when)
import           Control.Monad.ST            (runST, ST)

import           Data.Array.IArray           (Array, IArray, (!))
import qualified Data.Array.IArray           as Array
import           Data.Array.MArray           (MArray)
import qualified Data.Array.MArray           as MArray
import           Data.Array.ST               (STArray, STUArray, runSTArray, runSTUArray)
import           Data.Array.Unsafe           (unsafeFreeze)
import           Data.Array.Unboxed          (UArray)

import           Data.Word                   (Word8)

type Key = Word8
type Keys = UArray Key Key
type Values a = Array Word8 a

type STKeys s = STUArray s Key Key
type STValues s a = STArray s Key a

empty :: (IArray a e) => a Word8 e
empty = Array.array (1, 0) []

isEmpty :: (IArray a e) => a Word8 e -> Bool
isEmpty arr = let (a, b) = Array.bounds arr in a > b

              -- TODO: Switch to unsafeIndex?
-- | Finds the index of the given key in the array with a linear scan.
findIndex :: Key -> Keys -> Maybe Key
findIndex _ arr | isEmpty arr = Nothing
findIndex target arr = go 0
  where (_, size) = Array.bounds arr
        go !n | n <= size = if arr ! n == target then Just n else go (n + 1)
              | otherwise = Nothing

-- | Given a sorted array, returns the index containing the given element, if
-- any. With multiple equal elements, the index returned is
-- unspecified.
binarySearch :: Key -> Keys -> Maybe Key
binarySearch _ arr | isEmpty arr = Nothing
binarySearch target arr = go 0 $ snd (Array.bounds arr) + 1
  where go !from !to =
          let i = from + (to - from) `div` 2 in
          if | i + 1 == from || i == to -> Nothing
             | arr ! i == target        -> Just i
             | arr ! i < target         -> go (i + 1) to
             | otherwise                -> go from i



          -- TODO: Man, this code is ugly!

consKeys :: Key -> Keys -> Keys
consKeys x arr | isEmpty arr = Array.listArray (0, 0) [x]
consKeys x arr = runSTUArray $ do
  arr' <- MArray.newArray (0, size) 0
  MArray.writeArray arr' 0 x
  go arr' 0
  return arr'
    where size = snd (Array.bounds arr) + 1
          go arr' n = when (n < size) $ do
            MArray.writeArray arr' (n + 1) (arr ! n)
            go arr' (n + 1)

consValues :: a -> Values a -> Values a
consValues x arr | isEmpty arr = Array.listArray (0, 0) [x]
consValues x arr = runSTArray $ do
  arr' <- MArray.newArray (0, size) undefined
  MArray.writeArray arr' 0 x
  go arr' 0
  return arr'
    where size = snd (Array.bounds arr) + 1
          go arr' n = when (n < size) $ do
            MArray.writeArray arr' (n + 1) (arr ! n)
            go arr' (n + 1)

snocValues :: Values a -> a -> Values a
snocValues arr x | isEmpty arr = Array.listArray (0, 0) [x]
snocValues arr x = runSTArray $ do
  arr' <- MArray.newArray (0, size) undefined
  go arr' 0
  MArray.writeArray arr' size x
  return arr'
    where size = snd (Array.bounds arr) + 1
          go arr' n = when (n < size) $ do
            MArray.writeArray arr' n (arr ! n)
            go arr' (n + 1)

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
insert key value keys values | isEmpty keys =
  (Array.listArray (0, 0) [key], Array.listArray (0, 0) [value])
insert key value keys values = runST newArrays
  where size = snd (Array.bounds keys) + 1

        newArrays :: forall s. ST s (Keys, Values a)
        newArrays =
          do -- Remember: Array bounds are *inclusive*!
             newKeys   <- MArray.newArray (0, size) 0 -- initialized to 0 because unboxed
             newValues <- MArray.newArray (0, size) (error "uninitialized")
             go newKeys newValues 0
             keys'   <- unsafeFreeze newKeys
             values' <- unsafeFreeze newValues
             return (keys', values')

        go :: forall s. STKeys s -> STValues s a -> Key -> ST s ()
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
                  | otherwise = do -- keys ! n > key
                      write n key value
                      go' True $ n
                write n k v = MArray.writeArray newKeys n k >> MArray.writeArray newValues n v

-- | Takes an array of keys and a new key to add and produces a
-- 256-element byte-indexed array suitable for a Node48. Any unused
-- spaces are initialized to -1, the sentinel value I use for empty
-- cells in a Node48.
expandToByteKeyArray :: Key -> Keys -> Keys
expandToByteKeyArray newKey keys = runSTUArray $ do
  keys' <- MArray.newArray (0, 255) (-1)
  MArray.writeArray keys' newKey 0
  go keys' 0 -- copy over original keys array
  return keys'
  where (_, size) = Array.bounds keys
        go arr n = when (n <= size) $ do
          MArray.writeArray arr (keys ! n) (n + 1)
          go arr $ n + 1

-- | Takes an unboxed byte array of 255 keys and boxed array of values and
-- produces a boxed 256-element array of Maybe values.
expandKeysToValues :: Keys -> Values a -> Values (Maybe a)
expandKeysToValues keys values = runSTArray $ do
  values' <- MArray.newArray (0, 255) Nothing
  go values' 0
  return values'
  where (_, maxValue) = Array.bounds values
        (_, size) = Array.bounds keys
        go arr n = do
          when (keys ! n <= maxValue) $ MArray.writeArray arr n (Just $! values ! (keys ! n))
          when (n < 255) $ go arr (n + 1)
