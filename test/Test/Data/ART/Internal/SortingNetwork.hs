module Test.Data.ART.Internal.SortingNetwork where

import qualified Data.List                        as List
import qualified Data.Vector                      as V

import           Data.ART.Internal.SortingNetwork

prop_sort4 :: Int -> Int -> Int -> Int -> Bool
prop_sort4 a b c d =
  all (== vec) . map (V.modify (sort4 compare) . V.fromList) $ List.permutations [a, b, c, d]
  where vec = V.fromList $ List.sort [a, b, c, d]

prop_sort5 :: Int -> Int -> Int -> Int -> Int -> Bool
prop_sort5 a b c d e =
  all (== vec) . map (V.modify (sort5 compare) . V.fromList) $ List.permutations [a, b, c, d, e]
  where vec = V.fromList $ List.sort [a, b, c, d, e]
