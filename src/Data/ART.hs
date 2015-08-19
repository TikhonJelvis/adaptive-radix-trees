{-# OPTIONS_GHC -funbox-strict-fields #-}
module Data.ART where

import qualified Data.ByteString     as Byte
import           Data.Word           (Word8)
import           Data.Vector         (Vector)
import qualified Data.Vector         as Vector
import qualified Data.Vector.Unboxed as U

type Key = Byte.ByteString

data Node a = Node4   !(Vector (Maybe Key))    !(Vector (Maybe (ART a)))
              -- ^ Stores 0–4 children. Contains an unsorted array of
              -- 4 keys and an array of 4 children at the
              -- corresponding indices.
            | Node16  !(Vector (Maybe Key))    !(Vector (Maybe (ART a)))
              -- ^ Stores 5–16 children. Stores a sorted array of 16
              -- keys and an array of 16 children at the corresponding
              -- indices.
            | Node48  !(U.Vector (Maybe Word8)) !(Vector (Maybe (ART a)))
              -- ^ Stores 17–48 children. Stores an array of 256 bytes
              -- that indexes into an array of 48 children.
            | Node256 !(Vector (Maybe (ART a)))
              -- ^ Stores 49–256 children. Stores an array of 256
              -- children directly.

(!) :: Node a -> Key -> Maybe (ART a)
Node4 keys children ! key =
  Vector.elemIndex (Just key) keys >>= Vector.unsafeIndex children
Node16 keys children ! key = undefined

data ART a = Empty
           | Leaf !Key a
           | Branch !Key !Key !(Node a)
