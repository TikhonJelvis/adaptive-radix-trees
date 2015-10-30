{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DeriveGeneric    #-}
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
import           Control.DeepSeq                  (NFData (..), deepseq)
import           Control.Monad                    (guard, join)

import           Data.Array                       (Array)
import           Data.Array.IArray                ((!), (//))
import qualified Data.Array.IArray                as Array
import           Data.Array.Unboxed               (UArray)
import qualified Data.Array.Unboxed               as UArray

import           Data.Function                    (on)
import           Data.Word                        (Word8)

import           GHC.Generics                     (Generic)

import           Data.ART.Key                     (Chunk)

import qualified Data.ART.Internal.Array          as Array
import           Data.ART.Internal.SortingNetwork (sort4)

  -- TODO: Make the order of arguments in this API consistent!

type Size = Word8

             -- TODO: Figure out how to use unboxed vectors here!
-- | Stores elements using the appropriate inner node type.
data Children a =
    -- | Stores 1–4 children as arrays of up to 4 keys and values.
    N4   !Size !(UArray Chunk Chunk) !(Array Chunk a)
    -- | Stores 5–16 children as arrays of up to 16 keys and values.
  | N16  !Size !(UArray Chunk Chunk) !(Array Chunk a)
    -- | Stores 17–48 children as a chunk-indexed 256-element array of
    --   keys into an array of 48 children.
  | N48  !Size !(UArray Chunk Chunk) !(Array Chunk a)
    -- | Stores 49–256 children in a chunk-indexed array of 256 elements.
  | N256 !Size !(Array Chunk (Maybe a))
    deriving (Show, Eq)

             -- TODO: Figure out how to do this while preserving lazy values!
-- Helper functions that construct nodes taking care of
-- strictness.
n4, n16, n48 :: Size -> Array.Keys -> Array.Values a -> Children a
n4 size keys values = values `Array.seqValues` N4 size keys values
n16 size keys values = values `Array.seqValues` N16 size keys values
n48 size keys values = values `Array.seqValues` N48 size keys values

n256 :: Size -> Array.Values (Maybe a) -> Children a
n256 size values = values `Array.seqValues` N256 size values

instance NFData a => NFData (Children a) where
  rnf (N4 _ _ values)  = values `deepseq` ()
  rnf (N16 _ _ values) = values `deepseq` ()
  rnf (N48 _ _ values) = values `deepseq` ()
  rnf (N256 _ values)  = values `deepseq` ()

-- | How many children the current node stores.
size :: Children a -> Size
size (N4   size _ _) = size
size (N16  size _ _) = size
size (N48  size _ _) = size
size (N256 size _)   = size

-- | Gets the element for the given byte if it exists. Returns
--   'Nothing' otherwise.
get :: Children a -> Chunk -> Maybe a
get (N4 size keys values)  chunk = (values !) <$> Array.findIndex chunk keys
get (N16 size keys values) chunk = (values !) <$> Array.binarySearch chunk keys
get (N48  _ keys children) chunk =
  case keys ! chunk of
    i | i >= 48   -> Nothing
      | otherwise -> Just $! children ! i
get (N256 _ children) chunk = children ! chunk
{-# INLINE get #-}

-- | Given that the given key already has a value, update that value
-- with given function. If the value is not in the node, the node is
-- returned unchanged.
update :: (a -> a) -> Chunk -> Children a -> Children a
update f chunk (N4 size keys values) = n4 size keys newValues
  where newValues = case Array.findIndex chunk keys of
          Just i  -> values // [(i, f $! values ! i)]
          Nothing -> values
update f chunk (N16 size keys values) = n16 size keys newValues
  where newValues = case Array.binarySearch chunk keys of
          Just i  -> values // [(i, f $! values ! i)]
          Nothing -> values
update f chunk (N48 size keys values) = n48 size keys newValues
  where newValues | keys ! chunk < 0     = values
                  | otherwise = values // [(keys ! chunk, f $! values ! (keys ! chunk))]
update f chunk (N256 size values) = n256 size newValues
  where newValues | Just old <- values ! chunk = values // [(chunk, Just $! f old)]
                  | otherwise                  = values


-- | Insert the given value at the given key. If the key already has a
-- value, the given function is used to combine the old value and the
-- new value.
insertWith :: (a -> a -> a) -> Chunk -> a -> Children a -> Children a
insertWith f !chunk !value children
  | Just _ <- get children chunk = update (f value) chunk children
insertWith _ !chunk !value (N4 4 keys values) = n16 5 keys' values'
  where (keys', values') = uncurry (Array.insert chunk value) $ sort4 keys values
insertWith _ !chunk !value (N4 n keys values) = n4 (n + 1) keys' values'
  where (keys', values') = (Array.consKeys chunk keys, Array.consValues value values)

insertWith _ !chunk !value (N16 16 keys values) = n48 17 keys' values'
  where keys'   = Array.expandToByteKeyArray chunk keys
        values' = Array.consValues value values
insertWith _ !chunk !value (N16 n keys values) = n16 (n + 1) keys' values'
  where (keys', values') = Array.insert chunk value keys values

  -- TODO: The bug here was caused because chunks and the indicies
  -- internal to an N48 have the same type. I should probably wrap one
  -- of them.
insertWith _ !chunk !value (N48 48 keys values) =
  n256 49 $ Array.expandKeysToValues keys values // [(chunk, Just $! value)]
insertWith _ !chunk !value (N48 n keys values) = n48 (n + 1) keys' values'
  where keys'   = keys // [(chunk, n)]
        values' = Array.snocValues values value

insertWith f !chunk !value (N256 n values) = n256 (n + 1) $ values // [(chunk, Just $! value)]

insert :: Chunk -> a -> Children a -> Children a
insert = insertWith const

-- A utility function you probably shouldn't use in real code! (Yet?)
fromList :: [(Chunk, a)] -> Children a
fromList = foldr (uncurry insert) (N4 0 Array.empty Array.empty) . reverse


-- | Create a Node4 with the two given elements an everything else empty.
pair :: Chunk -> a -> Chunk -> a -> Children a
pair chunk1 v1 chunk2 v2 = N4 2 (Array.listArray (0,1) [chunk1, chunk2])
                                (Array.listArray (0,1) [v1, v2])

--                    -- TODO: Organize tests!
-- -- test_pairN4 = pairN4 k1 (Leaf k1 "abc") k2 (Leaf k2 "abc") == result
-- --   where k1 = Byte.pack ([1..6] ++ [10])
-- --         k2 = Byte.pack ([1..6] ++ [14])
-- --         result = Node 6 "\SOH\STX\ETX\EOT\ENQ\ACK" 2 (N4 (Node4 (fromList [10,14]) (fromList [Leaf "\SOH\STX\ETX\EOT\ENQ\ACK\n" "abc",Leaf "\SOH\STX\ETX\EOT\ENQ\ACK\SO" "abc"])))
