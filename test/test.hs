module Main where

import qualified Test.Data.ART.Key as Key

import           Test.Tasty

main = defaultMain tests

tests :: TestTree
tests = testGroup "Test" [keyPrefix]

keyPrefix = testGroup "Keys" Key.tests

-- binaryTrie :: TestTree
-- binaryTrie = testGroup "BinaryTrie" BinaryTrie.tests

-- trie :: TestTree
-- trie = testGroup "Trie" Trie.tests
