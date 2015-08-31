{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf          #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.ART where

import           Control.Applicative ((<$>))
import           Control.Monad       (guard, join)

import qualified Data.ByteString     as Byte
import qualified Data.List           as List
import           Data.Vector         (Vector, (!))
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as U
import           Data.Word           (Word8)

import           Prelude             hiding (lookup)

type Key    = Byte.ByteString
type Chunk  = Word8

type Prefix = Byte.ByteString
type Depth  = Int
type Size   = Int

(!~) :: Vector a -> Chunk -> a
vector !~ key = vector ! (fromIntegral key)

             -- TODO: Figure out how to use unboxed vectors here!

-- | Stores 1–4 children as an array of up to 4 key/child pairs.
data Node4 a = Node4 !(Vector Chunk) !(Vector (ART a)) deriving (Show, Eq)

-- | Stores 5–16 children as an array of up to 16 key/child pairs.
data Node16 a = Node16 !(Vector Chunk) !(Vector (ART a)) deriving (Show, Eq)

-- | Stores 17–48 children as a chunk-indexed 256-element array of
-- keys into an array of 48 children.
data Node48 a = Node48 !(Vector (Maybe Chunk)) !(Vector (ART a)) deriving (Show, Eq)

-- | Stores 49–256 children in a chunk-indexed array of 256 elements.
newtype Node256 a = Node256 (Vector (Maybe (ART a))) deriving (Show, Eq)

  -- TODO: Make this representation more efficient?
data Children a = N4   !(Node4   a)
                | N16  !(Node16  a)
                | N48  !(Node48  a)
                | N256 !(Node256 a)
                  deriving (Show, Eq)

data ART a = Empty
           | Leaf !Key a
           | Node !Depth !Prefix !Size !(Children a)
             deriving (Show, Eq)

get4 :: Node4 a -> Chunk -> Maybe (ART a)
get4 (Node4 chunks children) chunk = (children !) <$> V.findIndex (== chunk) chunks

get16 :: Node16 a -> Chunk -> Maybe (ART a)
get16 (Node16 chunks children) chunk = (children !) <$> go 0 (V.length chunks)
  where go !from !to =
          let i = from + (to - from) `div` 2
              k = chunks ! i
          in
          if | i + 1 == from || i == to -> Nothing
             | k == chunk             -> Just i
             | k < chunk             -> go (i + 1) to
             | otherwise             -> go from i

get48 :: Node48 a -> Chunk -> Maybe (ART a)
get48 (Node48 keys children) chunk = (children !~) <$> (keys !~ chunk)

get256 :: Node256 a -> Chunk -> Maybe (ART a)
get256 (Node256 children) chunk = children !~ chunk

-- | Does the given key start with the given prefix?
checkPrefix :: Depth -> Prefix -> Key -> Bool
checkPrefix depth prefix key = prefix' == Byte.take (Byte.length prefix') key
  where prefix' = Byte.take depth prefix


lookup :: Key -> ART a -> Maybe a
lookup key = go key 0
  where go !_   depth Empty                    = Nothing
        go !_   depth (Leaf k v)               = [v | key == k]
        go !key depth (Node _ prefix _ children) =
          case children of
            N4   nodes -> get4   nodes chunk >>= withPrefix
            N16  nodes -> get16  nodes chunk >>= withPrefix
            N48  nodes -> get48  nodes chunk >>= withPrefix
            N256 nodes -> get256 nodes chunk >>= withPrefix
          where chunk = Byte.head key
                withPrefix tree = guard (checkPrefix depth prefix key) >>
                                  go (Byte.tail key) (depth + 1) tree

data S4 a b c d = S4 !a !b !c !d

                -- TODO: figure out more efficient version?
-- | Extract the shared prefix, if any, from the two given keys.
splitKeys :: Key -> Key -> S4 Depth Prefix Key Key
splitKeys !k1 !k2 = S4 depth prefix (Byte.drop depth k1) (Byte.drop depth k2)
  where prefix = go k1 k2
        depth = Byte.length prefix
        go k1 k2
          | Byte.null k1 || Byte.null k2 = Byte.empty
          | chunk1 == chunk2             = Byte.cons chunk1 $ go rest1 rest2
          | otherwise                   = Byte.empty
          where Just (chunk1, rest1) = Byte.uncons k1
                Just (chunk2, rest2) = Byte.uncons k2

-- | Create a Node4 with the two given elements an everything else empty.
pairN4 :: Key -> ART a -> Key -> ART a -> ART a
pairN4 k1 v1 k2 v2 = Node depth prefix 2 (N4 children)
  where S4 depth prefix k1' k2' = splitKeys k1 k2
        (chunk1, chunk2) = (Byte.head k1', Byte.head k2')
        children = Node4 (V.fromList [chunk1, chunk2]) (V.fromList [v1, v2])

insertWith :: (a -> a -> a) -> Key -> a -> ART a -> ART a
insertWith f k v Empty       = Leaf k v
insertWith f k v (Leaf k' v')
  | k == k' = Leaf k (f v v')
  | otherwise = pairN4 k (Leaf k v) k' (Leaf k' v')

-- | Combine two nodes into one, using the given function to resolve conflicts.
mergeNodeWith :: (a -> a) -> Size -> Children a -> Size -> Children a -> Children a
mergeNodeWith f s1 n1 s2 n2 = case (n1, n2) of _ -> undefined


-- | A faster combine that does not do any path optimizations (lazy
-- expansion or path compression). Useful for insert.
combine' :: Prefix -> ART a -> Prefix -> ART a -> ART a
combine' !p1 !t1 !p2 !t2 = undefined

   -- TODO: Organize tests!
-- test_vec = map go [0..24] == map (flip List.elemIndex ls) [0..24]
--   where go x = binarySearch x $ V.fromList ls
--         ls = [11..13] ++ [15..17] ++ [20]
