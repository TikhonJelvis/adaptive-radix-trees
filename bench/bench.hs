module Main where

import           Data.Binary          (encode)
import           Data.ByteString.Lazy (toStrict)

import qualified Data.ART             as ART
import           Data.ART.Key         (Key)
import qualified Data.IntMap          as IntMap

import           Criterion.Main

main = defaultMain
       [ env values $ \ values ->
           bgroup "Creating 0..10000"
             [ bench "IntMap" $ nf IntMap.fromList $ fst values
             , bench "ART"    $ nf ART.fromList $ snd values
             ]
       ]
  where values :: IO ([(Int, Int)], [(Key, Int)])
        values = return ( let xs = [0..10000] in zip xs xs
                        , let xs = [0..10000] in map toKVPair $ zip xs xs
                        )

        toKVPair (k, v) = (toStrict $ encode k, v)
