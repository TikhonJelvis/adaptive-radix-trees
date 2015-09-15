{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ParallelListComp #-}
-- | The adaptive radix tree contains four different types of internal
-- nodes, depending on how many children they store:
--
--  * 1–4 children are stored as an array of up to 4 keys and a
--    corresponding array of up to 4 values
--
--  * 5–16 children are stored as a sorted array of up to 16 keys
--    and their corresponding values
--
--  * 17–48 children are stored as a 256-element byte array which
--    stores indices into an array of up to 48 children
--
--  * 49–256 children are stored directly in a 256 element array
--
-- This module contains code for working with all four of these as a
-- single 'Children' type. Each kind of node also stores the number of
-- children it currently contains which is used to grow or shrink
-- nodes when appropriate.
module Data.ART.Children where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (guard, join)

import           Data.Function                    (on)
import           Data.Vector                      (Vector, (!), (//))
import qualified Data.Vector                      as V

import           Data.ART.Key                     (Chunk, (!~))

import           Data.ART.Internal.SortingNetwork (sort5)
import qualified Data.ART.Internal.Vector         as AV

type Size = Int

             -- TODO: Figure out how to use unboxed vectors here!
-- | Stores elements using the appropriate inner node type.
data Children a =
    -- | Stores 1–4 children as an array of up to 4 key/child pairs.
    N4   !Size !(Vector (Chunk, a))
    -- | Stores 5–16 children as an array of up to 16 key/child pairs.
  | N16  !Size !(Vector (Chunk, a))
    -- | Stores 17–48 children as a chunk-indexed 256-element array of
    --   keys into an array of 48 children.
  | N48  !Size !(Vector (Maybe Chunk)) !(Vector a)
    -- | Stores 49–256 children in a chunk-indexed array of 256 elements.
  | N256 !Size !(Vector (Maybe a))
    deriving (Show, Eq)

-- | How many children the current node stores.
size :: Children a -> Size
size (N4   size _)   = size
size (N16  size _)   = size
size (N48  size _ _) = size
size (N256 size _)   = size

-- | Gets the element for the given byte if it exists. Returns
--   'Nothing' otherwise.
get :: Children a -> Chunk -> Maybe a
get (N4   _ pairs)         chunk = snd  <$> V.find ((== chunk) . fst) pairs
get (N16  _ pairs)         chunk = snd  <$> AV.binarySearch chunk pairs
get (N48  _ keys children) chunk = (children !~) <$> (keys !~ chunk)
get (N256 _ children)      chunk =  children !~ chunk
{-# INLINE get #-}

  -- TODO: Change to insertWith
insert :: Children a -> Chunk -> a -> Children a
insert (N4 4 pairs) !chunk !value =
  N16 5 $ V.modify (sort5 (compare `on` fst)) $ V.cons (chunk, value) pairs
insert (N4 n pairs) !chunk !value = N4 (n + 1) $ V.cons (chunk, value) pairs

insert (N16 16 pairs) !chunk !value = N48 17 keys (V.cons value $ V.map snd pairs)
  where keys = V.replicate 256 Nothing // changes
        changes = (fromIntegral chunk, Just 0) :
                  [(fromIntegral chunk, Just i) | chunk <- V.toList $ V.map fst pairs | i <- [1..16]]
insert (N16 n pairs) !chunk !value = N16 (n + 1) $ AV.insert (chunk, value) pairs

insert (N48 48 keys values) !chunk !value = N256 49 $ V.map update keys
  where update key = (values !) . fromIntegral <$> key
insert (N48 n keys values) !chunk !value = N48 (n + 1) keys' values'
  where keys'   = keys // [(fromIntegral chunk, Just $ fromIntegral n)]
        values' = V.snoc values value

insert (N256 n values) !chunk !value = N256 (n + 1) $ values // [(fromIntegral chunk, Just value)]

-- A utility function you probably shouldn't use in real code! (Yet?)
fromList :: [(Chunk, a)] -> Children a
fromList = foldr (\ (chunk, value) children -> insert children chunk value) (N4 0 V.empty)


-- | Create a Node4 with the two given elements an everything else empty.
pair :: Chunk -> a -> Chunk -> a -> Children a
pair chunk1 v1 chunk2 v2 = N4 2 $ V.fromList [(chunk1, v1), (chunk2, v2)]

                   -- TODO: Organize tests!
-- test_pairN4 = pairN4 k1 (Leaf k1 "abc") k2 (Leaf k2 "abc") == result
--   where k1 = Byte.pack ([1..6] ++ [10])
--         k2 = Byte.pack ([1..6] ++ [14])
--         result = Node 6 "\SOH\STX\ETX\EOT\ENQ\ACK" 2 (N4 (Node4 (fromList [10,14]) (fromList [Leaf "\SOH\STX\ETX\EOT\ENQ\ACK\n" "abc",Leaf "\SOH\STX\ETX\EOT\ENQ\ACK\SO" "abc"])))
