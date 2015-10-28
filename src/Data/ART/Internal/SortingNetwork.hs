{-# LANGUAGE RankNTypes #-}

-- | Efficient sorting networks for small vectors, based on diagrams
-- from
-- <http://www.anglefire.com/blog/ronz/blog/articles/999SortingNetworksReferen.html>.
module Data.ART.Internal.SortingNetwork where

import           Control.Monad.ST        (ST, runST)

import           Data.Array.MArray       (thaw)
import qualified Data.Array.MArray       as MArray
import           Data.Array.Unsafe       (unsafeFreeze)

import           Data.ART.Internal.Array (Key, Keys, STKeys, STValues, Values)

type Compare t = (t -> t -> Ordering)

swapIf :: Compare Key -> STKeys s -> STValues s a -> Key -> Key -> ST s ()
swapIf cmp keys values i j =
  do v_i <- MArray.readArray keys i
     v_j <- MArray.readArray keys j
     case cmp v_i v_j of
       GT -> swap keys >> swap values
       _  -> return ()
  where swap arr = do
          v_i <- MArray.readArray arr i
          v_j <- MArray.readArray arr j
          MArray.writeArray arr i v_j
          MArray.writeArray arr j v_i
{-# INLINE swapIf #-}

runSort :: forall a. (forall s. STKeys s -> STValues s a -> ST s ()) ->
           (Keys -> Values a -> (Keys, Values a))
runSort inPlace keys values = runST $ do
  keysST   <- thaw keys
  valuesST <- thaw values

  inPlace keysST valuesST

  keys'   <- unsafeFreeze keysST
  values' <- unsafeFreeze valuesST
  return (keys', values')
{-# INLINE runSort #-}

-- | A simple sorting network for 4 elements grabbed from Wikipedia.
sort4ByIndex :: Compare Key -> STKeys s -> STValues s a ->
                Key -> Key -> Key -> Key -> ST s ()
sort4ByIndex cmp keys values i j k l =
  do swap i k
     swap j l
     swap i j
     swap k l
     swap j k
  where swap = swapIf cmp keys values
{-# INLINABLE sort4ByIndex #-}

sort4 :: Keys -> Values a -> (Keys, Values a)
sort4 = runSort $ \ keys values -> sort4ByIndex compare keys values 0 1 2 3
{-# INLINABLE sort4 #-}

-- | A sorting network for 5 elements grabbed from the reference
-- linked at the top of this module.
sort5ByIndex :: Compare Key -> STKeys s -> STValues s a ->
                Key -> Key -> Key -> Key -> Key -> ST s ()
sort5ByIndex cmp keys values i j k l m =
  do swap i j
     swap k l
     swap j l
     swap k m
     swap i k
     swap j m
     swap j k
     swap l m
     swap k l
  where swap = swapIf cmp keys values
{-# INLINABLE sort5ByIndex #-}

sort5 :: Keys -> Values a -> (Keys, Values a)
sort5 = runSort $ \ keys values -> sort5ByIndex compare keys values 0 1 2 3 4
{-# INLINABLE sort5 #-}
