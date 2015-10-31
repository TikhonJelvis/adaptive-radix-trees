module Main where

import           Control.DeepSeq (deepseq)

import           Data.Binary          (encode)
import           Data.ByteString.Lazy (toStrict)

import           Data.ART             (ART)
import qualified Data.ART             as ART

tree :: [Int] -> ART Int
tree values = ART.fromList $ zip (map (toStrict . encode) values) values

main = tree values `deepseq` putStrLn "done"
  where values = [0..10000]
