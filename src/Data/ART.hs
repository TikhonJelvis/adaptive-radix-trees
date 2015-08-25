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

(!~) :: Vector a -> Chunk -> a
vector !~ key = vector ! (fromIntegral key)

             -- TODO: Figure out how to use unboxed vectors here!

-- | Stores 1–4 children as an array of key/child pairs. This is
-- handled efficiently under the hood because an unboxed vector of
-- pairs is represented as a pair of unboxed vectors.
newtype Node4 a = Node4 (Vector (Chunk, ART a)) deriving (Show, Eq)

-- | Stores 5–16 children as an array of key/child pairs. The relevant
-- key can be found with a binary search.
newtype Node16 a = Node16 (Vector (Chunk, ART a)) deriving (Show, Eq)

-- | Stores 17–48 children as a chunk-indexed 256-element array of
-- keys into an array of 48 children.
data Node48 a = Node48 !(Vector (Maybe Chunk)) !(Vector (ART a)) deriving (Show, Eq)

-- | Stores 49–256 children in a chunk-indexed array of 256 elements.
newtype Node256 a = Node256 (Vector (Maybe (ART a))) deriving (Show, Eq)

get4 :: Node4 a -> Chunk -> Maybe (ART a)
get4 (Node4 pairs) chunk = snd <$> V.find ((== chunk) . fst) pairs

get16 :: Node16 a -> Chunk -> Maybe (ART a)
get16 (Node16 pairs) chunk = go 0 (V.length pairs)
  where go !from !to =
          let i      = from + (to - from) `div` 2
              (k, v) = pairs ! i
          in
          if | i + 1 == from || i == to -> Nothing
             | k == chunk             -> Just v
             | k < chunk             -> go (i + 1) to
             | otherwise             -> go from i

get48 :: Node48 a -> Chunk -> Maybe (ART a)
get48 (Node48 keys children) chunk = (children !~) <$> (keys !~ chunk)

get256 :: Node256 a -> Chunk -> Maybe (ART a)
get256 (Node256 children) chunk = children !~ chunk

   -- TODO: Organize tests!
-- test_vec = map go [0..24] == map (flip List.elemIndex ls) [0..24]
--   where go x = binarySearch x $ V.fromList ls
--         ls = [11..13] ++ [15..17] ++ [20]

data ART a = Empty
           | Leaf Key a
           | N4   !(Node4   a)
           | N16  !(Node16  a)
           | N48  !(Node48  a)
           | N256 !(Node256 a)
             deriving (Show, Eq)
