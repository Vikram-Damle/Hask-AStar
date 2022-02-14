module Heuristics where

import Types
import Utils
import Data.Array (assocs)

misplacedTile :: BoardState -> Int
misplacedTile cs = ct
  where
    xs = fromBoard cs
    count = zipWith (==) xs [0..8]
    ct = length . filter (/= True) $ count


manhattanDist :: BoardState -> Int
manhattanDist cs = sum dists
  where
    manDist (a, b) (c, d) = abs(a-c) + abs(b-d)
    bs = assocs (fmap (indexToCoords . fromEnum) cs)
    dists = map (uncurry manDist) bs



