module Test.Data.Internal.Array where

import           Data.ART.Internal.Array

import           Data.Foldable           (fromList, toList)
import qualified Data.List               as List
import           Data.Maybe              (isJust)

import           Data.Array.IArray       (Array)
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

prop_findIndex (NonEmptyList keys) = forAll $ choose (0, length keys - 1) $ \ i ->
  findIndex (keys !! i) (fromList keys) == Just i

prop_findIndexMissing key keys = findIndex key filtered == Nothing
  where filtered = fromList $ filter (/= key) keys

prop_binarySearch (OrderedList []) = forAll $ choose (0, 100) $ \ k ->
  binarySearch k (fromList []) == Nothing
prop_binarySearch (OrderedList keys) = forAll $ choose (0, length keys - 1) $ \ i ->
  findIndex (keys !! i) (fromList keys) == Just i

prop_binarySearchMissing (OrderedList keys) = binarySearch key filtered == Nothing
  where filtered = fromList $ filter (/= key) keys

prop_insert key (OrderedList keys) value =
  and [ isOrdered $ toList newKeys
      , isJust (findIndex key newKeys)
      , bounds newKeys == bounds newValues
      , List.findIndex (== key) (toList newKeys) ==
        List.findIndex (== value) (toList newValues)
      ]
  where (newKeys, newValues) = insert key value (fromList keys) (fromList values)

isOrdered []       = True
isOrdered [x]      = True
isOrdered (x:y:xs) = x <= y && isOrdered (y:xs)
