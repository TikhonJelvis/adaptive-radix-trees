module Test.Data.ART.Internal.Array where

import           Data.ART.Internal.Array

import qualified Data.List               as List
import           Data.Maybe              (isJust)

import           Data.Array.IArray       (Array, IArray, (!))
import qualified Data.Array.IArray       as Array

import           Test.QuickCheck
import           Test.Tasty
import qualified Test.Tasty.QuickCheck   as QC

tests :: [TestTree]
tests = [ QC.testProperty "findIndex" prop_findIndex
        , QC.testProperty "findIndex missing" prop_findIndexMissing
        , QC.testProperty "binarySearch" prop_binarySearch
        , QC.testProperty "binarySearch missing" prop_binarySearchMissing
        , QC.testProperty "insert" prop_insert
        ]

fromList :: (IArray a e) => [e] -> a Key e
fromList [] = Array.listArray (1, 0) []
fromList ls = Array.listArray (0, List.genericLength ls - 1) ls

prop_findIndex (NonEmpty keys') =
  let keys = List.nub keys' in
  forAll (choose (0, List.genericLength keys - 1)) $ \ i ->
  findIndex (keys !! fromIntegral i) (fromList keys) == Just i

prop_findIndexMissing key keys = findIndex key filtered == Nothing
  where filtered = fromList $ filter (/= key) keys

-- I couldn't find a convenient way to combine OrderedList and NonEmptyList
prop_binarySearch (Ordered []) = forAll (choose (0, 100)) $ \ k ->
  binarySearch k (fromList []) == Nothing
prop_binarySearch (Ordered keys') =
  let keys = List.nub keys' in
  forAll (choose (0, List.genericLength keys - 1)) $ \ i ->
  findIndex (keys !! fromIntegral i) (fromList keys) == Just i

prop_binarySearchMissing key (Ordered keys) = binarySearch key filtered == Nothing
  where filtered = fromList $ filter (/= key) keys

prop_insert key (Ordered keys) value =
  and [ isOrdered $ Array.elems newKeys
      , isJust (findIndex key newKeys)
      , Array.bounds newKeys == Array.bounds newValues
      , any (== value) [ newValues ! fromIntegral i |
                         i <- List.findIndices (== key) $ Array.elems newKeys
                       ]
      ]
  where (newKeys, newValues) = insert key value (fromList keys) (fromList keys)

isOrdered []       = True
isOrdered [x]      = True
isOrdered (x:y:xs) = x <= y && isOrdered (y:xs)
