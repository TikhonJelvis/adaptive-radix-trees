module Test.Data.ART.Key where

import qualified Data.ByteString as Byte

import           Test.QuickCheck
import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

import           Data.ART.Key

tests :: [TestTree]
tests = [ QC.testProperty "prefixes equal" prop_prefixEqual
        , QC.testProperty "prefixes empty" prop_prefixEmpty
        , QC.testProperty "prefixes" prop_prefix
        ]
        
newtype Pack = Pack Byte.ByteString

instance QC.Arbitrary Pack where arbitrary = Pack . Byte.pack <$> QC.arbitrary

instance Show Pack where show (Pack bs) = show bs

prop_prefixEqual (Pack bs) = sharedPrefix bs bs == bs

prop_prefixEmpty (Pack bs) = sharedPrefix bs Byte.empty == Byte.empty

prop_prefix (Pack prefix) (Pack suffix₁) (Pack suffix₂) = start == prefix
  where start = Byte.take (Byte.length prefix) $ sharedPrefix s₁ s₂

        s₁ = Byte.append prefix suffix₁
        s₂ = Byte.append prefix suffix₂
