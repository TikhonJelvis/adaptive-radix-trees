{-# LANGUAGE ScopedTypeVariables #-}
module Test.Data.ART.Children where

import           Data.ART.Children
import           Data.ART.Key          (Chunk)

import           Test.QuickCheck
import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

tests :: [TestTree]
tests = [ QC.testProperty "get" prop_get ]

prop_get (NonEmpty (ls' :: [(Chunk, Bool)])) =
  let ls = take 256 ls' in
  forAll (choose (0, length ls' - 1)) $ \ i ->
  let (key, value) = ls !! i
      children = fromList ls
  in
  get children key == Just value
