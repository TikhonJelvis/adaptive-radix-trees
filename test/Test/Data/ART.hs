module Test.Data.ART where

import qualified Data.ART              as ART

import           Test.QuickCheck
import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import           Test.Data.ART.Key     (Pack (..))

tests :: [TestTree]
tests = [ QC.testProperty "set get empty" prop_setGetEmpty
        , QC.testProperty "set get random" prop_setGetRandom]

-- | Simplest case: insert "abc" into an empty tree and get it back out.
prop_setGetEmpty (Pack key) =
  ART.lookup key (ART.insert key "abc" ART.Empty) == Just "abc"

prop_setGetRandom (Pack key) = forAll (listOf arbitrary) $ \ ls ->
  let tree = ART.fromList . zip (map unpack ls) $ repeat "def" in
  ART.lookup key (ART.insert key "abc" tree) == Just "abc"
