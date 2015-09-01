module Test.Data.ART where

import           Data.ART
import qualified Data.ByteString as Byte

prop_prefixEqual bs = sharedPrefix bs bs == bs

prop_prefixEmpty bs = sharedPrefix bs Byte.empty == Byte.empty

prop_prefix prefix suffix₁ suffix₂ = sharedPrefix s₁ s₂ == prefix
  where s₁ = Byte.append prefix suffix₁
        s₂ = Byte.append prefix suffix₂
