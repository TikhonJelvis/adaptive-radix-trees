{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf          #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.ART where

import           Control.Applicative              ((<$>))
import           Control.DeepSeq                  (NFData (..))
import           Control.Monad                    (guard, join)

import qualified Data.List                        as List
import           Data.Maybe                       (fromMaybe)
import           Data.Vector                      (Vector, (!))
import qualified Data.Vector                      as V
import qualified Data.Vector.Unboxed              as U
import           Data.Word                        (Word8)

import           GHC.Generics                     (Generic)

import           Prelude                          hiding (lookup)

import           Data.ART.Internal.SortingNetwork

import           Data.ART.Children                (Children, Size)
import qualified Data.ART.Children                as Children
import           Data.ART.Key                     (Chunk, Depth, Key, Prefix,
                                                   (!~))
import qualified Data.ART.Key                     as Key

data ART a = Empty
           | Leaf !Key a
           | Node !Depth !Prefix !(Children (ART a))
           deriving (Show, Eq, Generic, NFData)

  -- TODO: Do we need depth in Node, or could we just use Byte.length prefix?
-- | Get the value associated with the given key, if any.
lookup :: Key -> ART a -> Maybe a
lookup !key = go
  where go Empty                        = Nothing
        go (Leaf k v)                   = [v | key == k]
        go (Node depth prefix children) =
          do next <- Children.get children $ Key.getChunk key depth
             guard $ Key.checkPrefix depth prefix key
             go next

insertWith :: (a -> a -> a) -> Key -> a -> ART a -> ART a
insertWith _ k v Empty = Leaf k v
insertWith f k v (Leaf k' v')
  | k == k'   = Leaf k (f v v')
  | otherwise = combine k (Leaf k v) k' (Leaf k' v')
insertWith f k v node@(Node depth prefix children)
  | Key.checkPrefix depth prefix k =
    Node depth prefix $ Children.insertWith (mergeWith f) chunk (Leaf k v) children
  | otherwise = combine k (Leaf k v) prefix node
  where chunk = Key.getChunk k depth
        mergeWith f _ = insertWith f k v

insert :: Key -> a -> ART a -> ART a
insert = insertWith const

-- Convenience function for testing that you probably shouldn't use?
fromList :: [(Key, a)] -> ART a
fromList = foldr (uncurry insert) Empty

-- | Combine two nodes with disjoint prefixes into a single tree by
-- creating a new @N4@ for the pair of keys.
combine :: Key -> ART a -> Key -> ART a -> ART a
combine !k1 t1 !k2 t2 = Node depth prefix $ Children.pair c t1 c' t2
  where (c, c') = (Key.getChunk k1 depth, Key.getChunk k2 depth)
        prefix  = Key.sharedPrefix k1 k2
        depth   = Key.length prefix

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
