{-# LANGUAGE BangPatterns #-}
-- | Currently, ART trees are indexed by 'ByteString's, with each
-- chunk of the key as a 'Word8'.
module Data.ART.Key where

import qualified Data.ByteString as Bytes
import           Data.Word       (Word8)

type Key   = Bytes.ByteString
type Chunk = Word8

type Prefix = Bytes.ByteString
type Depth  = Int

-- | Checks whether the first @depth@ bytes of the key match the first
--   @depth@ bytes of the prefix.
checkPrefix :: Depth -> Prefix -> Key -> Bool
checkPrefix depth prefix key = Bytes.take depth prefix == Bytes.take depth key

-- | Extracts the prefix shared between the two keys, if any. The
--   prefix may be empty.
sharedPrefix :: Key -> Key -> Prefix
sharedPrefix !k1 !k2 = Bytes.take (go 0) k1
  where limit = min (Bytes.length k1) (Bytes.length k2)
        go n | n == limit                           = n
             | Bytes.index k1 n == Bytes.index k2 n = go (n + 1)
             | otherwise                            = n

-- | The length of the prefix in bytes.
length :: Prefix -> Int
length = Bytes.length

-- | Gets the chunk of the key at the given depth. A depth out of
--   bounds results in an exception.
getChunk :: Key -> Depth -> Chunk
getChunk = Bytes.index
