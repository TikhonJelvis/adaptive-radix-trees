module Test.Data.ART where

import           Control.Monad         (replicateM)

import qualified Data.ART              as ART

import           Test.QuickCheck
import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import           Test.Data.ART.Key     (Pack (..))

tests :: [TestTree]
tests = [ QC.testProperty "set get empty" prop_setGetEmpty
        , QC.testProperty "set get random" prop_setGetRandom
        , QC.testProperty "set get big" prop_setGetRandom]

-- | Simplest case: insert "abc" into an empty tree and get it back out.
prop_setGetEmpty (Pack key) =
  ART.lookup key (ART.insert key "abc" ART.Empty) == Just "abc"

-- | Insert "abc" into a tree full of random keys and get it back out.
prop_setGetRandom (Pack key) = forAll (listOf arbitrary) $ \ ls ->
  let tree = ART.fromList . zip (map unpack ls) $ repeat "def" in
  ART.lookup key (ART.insert key "abc" tree) == Just "abc"

-- | Insert "abc" into a tree with 1000000 random keys and get it back out.
prop_setGetBig (Pack key) = forAll (replicateM 1000000 arbitrary) $ \ ls ->
  let tree = ART.fromList . zip (map unpack ls) $ repeat "def" in
  ART.lookup key (ART.insert key "abc" tree) == Just "abc"
