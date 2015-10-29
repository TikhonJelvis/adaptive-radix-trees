{-# LANGUAGE ScopedTypeVariables #-}
module Test.Data.ART.Internal.Array where

import           Data.ART.Internal.Array

import           Data.Function           (on)
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
        , QC.testProperty "consValues" prop_consValues
        , QC.testProperty "snocValues" prop_snocValues
        , QC.testProperty "consKeys" prop_consKeys
        , QC.testProperty "expandToByteKeyArray" prop_expandToByteKeyArray
        , QC.testProperty "expandKeysToValues" prop_expandKeysToValues
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

prop_consValues (value :: Int) values =
  (value : values) == Array.elems (consValues value valuesArray)
  where valuesArray = Array.listArray (0, List.genericLength values - 1) values

prop_consKeys (key :: Key) keys =
  (key : keys) == Array.elems (consKeys key keysArray)
  where keysArray = Array.listArray (0, List.genericLength keys - 1) keys

prop_snocValues (value :: Int) values =
  (values ++ [value]) == Array.elems (snocValues valuesArray value)
  where valuesArray = Array.listArray (0, List.genericLength values - 1) values

prop_expandToByteKeyArray (NonEmpty keys') =
  forAll (arbitrary `suchThat` (`notElem` keys')) $ \ (newKey :: Key) ->
  let keys = take 48 . List.nub $ filter (/= newKey) keys'
      keysArray = Array.listArray (0, List.genericLength keys - 1) keys
      expanded = expandToByteKeyArray newKey keysArray
  in
  and [ Array.bounds expanded == (0, 255)
      , all (\ k -> expanded ! k >= 0) keys'
      ]

prop_expandKeysToValues (NonEmpty (kvPairs' :: [(Key, Int)])) =
  and [ Array.bounds expanded == (0, 255)
      , all (\ (k, v) -> expanded ! k == Just v) kvPairs
      ]
  where kvPairs = List.nubBy ((==) `on` fst) kvPairs'
        size = List.genericLength kvPairs - 1
        keys = Array.listArray (0, size) $ map fst kvPairs
        values = Array.listArray (0, size) $ map snd kvPairs
        expanded = expandKeysToValues keys values

isOrdered []       = True
isOrdered [x]      = True
isOrdered (x:y:xs) = x <= y && isOrdered (y:xs)
