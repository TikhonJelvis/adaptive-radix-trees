module Main where

import           Data.Array           (Array, (//))
import qualified Data.Array           as Array
import           Data.Binary          (encode)
import           Data.ByteString.Lazy (toStrict)

import           Data.ART             (ART)
import qualified Data.ART             as ART
import           Data.ART.Children    (Children)
import qualified Data.ART.Children    as Children
import           Data.ART.Key         (Key, Chunk)
import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IntMap

import           Criterion.Main

main = defaultMain
       [ env values $ \ values ->
           bgroup "Creating 0..10000"
             [ bench "IntMap" $ nf IntMap.fromList $ fst values
             , bench "ART"    $ nf ART.fromList $ snd values
             ]
       , env children $ \ children ->
           bgroup "Inserting into children."
             [ bench "N256" $ nf (Children.insert 42 57) $ fst children
             , bench "Raw Data.Array 256" $ nf (// [(42, 57)]) $ snd children
             ]
       , env trees $ \ trees ->
           bgroup "Inserting into 10k"
             [ bench "IntMap" $ nf (IntMap.insert 10001 42) $ fst trees
             , bench "ART   " $ nf (ART.insert (toStrict $ encode (10001 :: Integer)) 42) $ snd trees
             ]
       ]
  where values :: IO ([(Int, Int)], [(Key, Int)])
        values = return ( let xs = [0..10000] in zip xs xs
                        , let xs = [0..10000] in map toKVPair $ zip xs xs
                        )

        children :: IO (Children Int, Array Chunk Int)
        children = return ( Children.fromList $ zip [0..255] [0..255]
                          , Array.listArray (0, 255) [0..255]
                          )

        trees :: IO (IntMap Int, ART Int)
        trees = return ( IntMap.fromList             (zip [0..10000] [0..10000])
                       , ART.fromList (map toKVPair $ zip [0..(10000 :: Integer)] [0..10000])
                       )

        toKVPair (k, v) = (toStrict $ encode k, v)
