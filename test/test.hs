module Main where

import qualified Test.Data.ART                         as ART
import qualified Test.Data.ART.Internal.Array          as Array
import qualified Test.Data.ART.Internal.SortingNetwork as Sort
import qualified Test.Data.ART.Key                     as Key

import           Test.Tasty

main = defaultMain tests

tests :: TestTree
tests = testGroup "Test" [ testGroup "Keys" Key.tests
                         , testGroup "ART" ART.tests
                         , testGroup "Array" Array.tests
                         , testGroup "SortingNetwork" Sort.tests
                         ]
