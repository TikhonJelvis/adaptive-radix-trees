{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf          #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.ART where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (guard, join)

import qualified Data.List                        as List
import           Data.Maybe                       (fromMaybe)
import           Data.Vector                      (Vector, (!))
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as U
import           Data.Word                        (Word8)

import           Prelude                          hiding (lookup)

import           Data.ART.Internal.SortingNetwork
import           Data.ART.Internal.Vector

import           Data.ART.Key                     (Chunk, Depth, Key, Prefix,
                                                   (!~))
import qualified Data.ART.Key                     as Key

data ART a = Empty
           | Leaf !Key a
           | Node !Depth !Prefix !Size !(Children a)
             deriving (Show, Eq)

  -- TODO: Factor this logic out into a Data.ART.Children module?
get4 :: Node4 a -> Chunk -> Maybe (ART a)
get4 (Node4 chunks children) chunk = (children !) <$> V.findIndex (== chunk) chunks

get16 :: Node16 a -> Chunk -> Maybe (ART a)
get16 (Node16 chunks children) chunk = (children !) <$> binarySearch chunk chunks

get48 :: Node48 a -> Chunk -> Maybe (ART a)
get48 (Node48 keys children) chunk = (children !~) <$> (keys !~ chunk)

get256 :: Node256 a -> Chunk -> Maybe (ART a)
get256 (Node256 children) chunk = children !~ chunk

getChild :: Children a -> Chunk -> ART a
getChild children chunk = fromMaybe Empty $ go children chunk
  where go (N4   nodes) = get4   nodes
        go (N16  nodes) = get16  nodes
        go (N48  nodes) = get48  nodes
        go (N256 nodes) = get256 nodes

  -- TODO: Optimization idea: copy to mutable array directly with
  -- extra child, sort in place, unsafeFreeze.
grow4 :: Chunk -> ART a -> Node4 a -> Node16 a
grow4 chunk child (Node4 chunks children) =
  Node16 (V.modify (sort5 compare) $ V.cons chunk chunks) (V.cons child children)

  -- TODO: Do we need depth in Node, or could we just use Byte.length prefix?
lookup :: Key -> ART a -> Maybe a
lookup key = go key
  where go _    Empty                          = Nothing
        go _    (Leaf k v)                     = [v | key == k]
        go !key (Node depth prefix _ children) =
          do let next = getChild children $ Key.getChunk key depth
             guard (Key.checkPrefix depth prefix key)
             go key next

-- | Create a Node4 with the two given elements an everything else empty.
pairN4 :: Key -> ART a -> Key -> ART a -> ART a
pairN4 k1 v1 k2 v2 = Node depth prefix 2 (N4 children)
  where prefix = Key.sharedPrefix k1 k2
        depth = Key.length prefix
        (chunk1, chunk2) = (Key.getChunk k1 depth, Key.getChunk k2 depth)
        children = Node4 (V.fromList [chunk1, chunk2]) (V.fromList [v1, v2])

                   -- TODO: Organize tests!
-- test_pairN4 = pairN4 k1 (Leaf k1 "abc") k2 (Leaf k2 "abc") == result
--   where k1 = Byte.pack ([1..6] ++ [10])
--         k2 = Byte.pack ([1..6] ++ [14])
--         result = Node 6 "\SOH\STX\ETX\EOT\ENQ\ACK" 2 (N4 (Node4 (fromList [10,14]) (fromList [Leaf "\SOH\STX\ETX\EOT\ENQ\ACK\n" "abc",Leaf "\SOH\STX\ETX\EOT\ENQ\ACK\SO" "abc"])))

insertWith :: (a -> a -> a) -> Key -> a -> ART a -> ART a
insertWith f k v Empty        = Leaf k v
insertWith f k v (Leaf k' v')
  | k == k'    = Leaf k (f v v')
  | otherwise = pairN4 k (Leaf k v) k' (Leaf k' v')
insertWith f k v (Node depth prefix size children)
  | Key.checkPrefix depth prefix k = Node depth prefix size newChildren
  where newChildren = undefined

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
