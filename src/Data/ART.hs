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

import           Data.ART.Children                (Children, Size)
import qualified Data.ART.Children                as Children
import           Data.ART.Key                     (Chunk, Depth, Key, Prefix,
                                                   (!~))
import qualified Data.ART.Key                     as Key

data ART a = Empty
           | Leaf !Key a
           | Node !Depth !Prefix !(Children (ART a))
           deriving (Show, Eq)

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
insertWith f k v Empty = Leaf k v
insertWith f k v (Leaf k' v')
  | k == k'    = Leaf k (f v v')
  | otherwise = Node depth prefix $ Children.pair c (Leaf k v) c' (Leaf k' v')
  where (c, c') = (Key.getChunk k depth, Key.getChunk k' depth)
        prefix  = Key.sharedPrefix k k'
        depth   = Key.length prefix
insertWith f k v (Node depth prefix children)
  | Key.checkPrefix depth prefix k = Node depth prefix newChildren
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
