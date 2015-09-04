-- | Efficient sorting networks for small vectors, based on diagrams
-- from
-- <http://www.anglefire.com/blog/ronz/blog/articles/999SortingNetworksReferen.html>.
module Data.ART.Internal.SortingNetwork where

import           Control.Monad.Primitive     (PrimMonad, PrimState)

import qualified Data.Vector                 as V
import qualified Data.Vector.Generic.Mutable as MV
import           Data.Vector.Generic.Mutable (MVector)

type Compare t = (t -> t -> Ordering)

swapIf :: (MVector v a, PrimMonad m) => Compare a -> v (PrimState m) a -> Int -> Int -> m ()
swapIf cmp a i j = 
  do v_i <- MV.unsafeRead a i
     v_j <- MV.unsafeRead a j
     case cmp v_i v_j of
       GT -> MV.unsafeSwap a i j
       _  -> return ()
{-# INLINE swapIf #-}

-- | A simple sorting network for 4 elements grabbed from Wikipedia.
sort4ByIndex :: (MVector v a, PrimMonad m) => Compare a -> v (PrimState m) a -> 
               Int -> Int -> Int -> Int -> m ()
sort4ByIndex cmp a i j k l = 
  do swap i k
     swap j l
     swap i j
     swap k l
     swap j k
  where swap = swapIf cmp a
{-# INLINABLE sort4ByIndex #-}

sort4ByOffset :: (MVector v a, PrimMonad m) => Compare a -> v (PrimState m) a -> Int -> m ()
sort4ByOffset cmp a off = sort4ByIndex cmp a off (off + 1) (off + 2) (off + 3)
{-# INLINABLE sort4ByOffset #-}

sort4 :: (MVector v a, PrimMonad m) => Compare a -> v (PrimState m) a -> m ()
sort4 cmp a = sort4ByOffset cmp a 0
{-# INLINABLE sort4 #-}

sort5ByIndex :: (MVector v a, PrimMonad m) => Compare a -> v (PrimState m) a -> 
               Int -> Int -> Int -> Int -> Int -> m ()
sort5ByIndex cmp a i j k l m = 
  do swap j k
     swap l m
     swap j l
     swap i k
     swap k m
     swap i m
     swap i j
     swap k l
     swap j k
  where swap = swapIf cmp a
{-# INLINABLE sort5ByIndex #-}

sort5ByOffset :: (MVector v a, PrimMonad m) => Compare a -> v (PrimState m) a -> Int -> m ()
sort5ByOffset cmp a off = sort5ByIndex cmp a off (off + 1) (off + 2) (off + 3) (off + 4)
{-# INLINABLE sort5ByOffset #-}

sort5 :: (MVector v a, PrimMonad m) => Compare a -> v (PrimState m) a -> m ()
sort5 cmp a = sort5ByOffset cmp a 0
{-# INLINABLE sort5 #-}
