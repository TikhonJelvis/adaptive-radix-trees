module Test.Data.ART.Internal.SortingNetwork where

import           Control.Monad                    (join)

import qualified Data.Array.IArray                as Array
import qualified Data.List                        as List

import           Data.ART.Internal.Array          (Key)
import           Data.ART.Internal.SortingNetwork

import           Test.Tasty
import qualified Test.Tasty.QuickCheck   as QC

tests :: [TestTree]
tests = [ QC.testProperty "sort4" prop_sort4
        , QC.testProperty "sort5" prop_sort5
        ]

prop_sort4 :: Key -> Key -> Key -> Key -> Bool
prop_sort4 a b c d =
  all (== (keys, values)) . map toArray $ List.permutations [a, b, c, d]
  where toArray ls = sort4 (Array.listArray (0,3) ls) (Array.listArray (0,3) ls)
        keys = Array.listArray (0,3) $ List.sort [a, b, c, d]
        values = Array.listArray (0,3) $ List.sort [a, b, c, d]

prop_sort5 :: Key -> Key -> Key -> Key -> Key -> Bool
prop_sort5 a b c d e =
  all (== (keys, values)) . map toArray $ List.permutations [a, b, c, d, e]
  where toArray ls = sort5 (Array.listArray (0,4) ls) (Array.listArray (0,4) ls)
        keys = Array.listArray (0,4) $ List.sort [a, b, c, d, e]
        values = Array.listArray (0,4) $ List.sort [a, b, c, d, e]
