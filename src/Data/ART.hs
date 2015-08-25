{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf   #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.ART where

import           Control.Applicative ((<$>))

import qualified Data.ByteString     as Byte
import qualified Data.List           as List
import           Data.Vector         (Vector, (!))
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import           Data.Word           (Word8)

type Key   = Byte.ByteString
type Chunk = Word8

             -- TODO: Figure out how to use unboxed vectors here!

-- | Stores 1–4 children as an array of key/child pairs. This is
-- handled efficiently under the hood because an unboxed vector of
-- pairs is represented as a pair of unboxed vectors.
newtype Node4 a = Node4 (Vector (Chunk, ART a))

-- | Stores 5–16 children as an array of key/child pairs. The relevant
-- key can be found with a binary search.
newtype Node16 a = Node16 (Vector (Chunk, ART a))

-- | Stores 17–48 children as a chunk-indexed 256-element array of
-- keys into an array of 48 children.
data Node48 a = Node48 !(Vector (Maybe Int)) !(Vector (ART a))

-- | Stores 49–256 children in a chunk-indexed array of 256 elements.
newtype Node256 a = Node256 (Vector (Maybe (ART a)))

get4 :: Eq a => Node4 a -> Chunk -> Maybe (ART a)
get4 (Node4 pairs) chunk = snd <$> V.find ((== chunk) . fst) pairs

binarySearch :: Chunk -> Vector Chunk -> Maybe Int
binarySearch goal vector = go 0 (V.length vector)
  where go !from !to = let i = from + (to - from) `div` 2 in
          if | i + 1 == from || i == to -> Nothing
             | vector ! i == goal     -> Just i
             | vector ! i < goal     -> go (i + 1) to
             | otherwise             -> go from i

   -- TODO: Organize tests!
test_vec = map go [0..24] == map (flip List.elemIndex ls) [0..24]
  where go x = binarySearch x $ V.fromList ls
        ls = [11..13] ++ [15..17] ++ [20]

get16 :: Eq a => Node16 a -> Chunk -> Maybe (ART a)
get16 (Node16 pairs) chunk = undefined

data ART a = Empty
           | Leaf Key a
           | N4   !(Node4   a)
           | N16  !(Node16  a)
           | N48  !(Node48  a)
           | N256 !(Node256 a)
