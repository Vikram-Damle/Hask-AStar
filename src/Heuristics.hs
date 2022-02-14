module Heuristics (
    hNullHeur
  , hMispTile
  , hManhDist
  , hNMspTile
  , hNManDist
  , heuristics
) where

import Types
import Utils
import Change

import Data.Array (assocs)



hNullHeur :: Heuristic
hMispTile :: Heuristic
hManhDist :: Heuristic
hNMspTile :: Heuristic
hNManDist :: Heuristic
hNullHeur = (const 0, "Null Heuristic")
hMispTile = (misplacedTile, "Misplaced Tile")
hManhDist = (manhattanDist, "Manhattan Distance")
hNMspTile = (neighbourMpTile, "Neighbour Misplaced Tile")
hNManDist = (neighbourManhattan, "Neighbour Manhattan Distance")


heuristics :: [Heuristic]
heuristics = [hNullHeur, hMispTile, hManhDist, hNMspTile, hNManDist]

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

neighbourCost :: CostFunc -> BoardState -> Int
neighbourCost h = (+1) . minimum . map h . getChildren
  
neighbourManhattan :: BoardState -> Int
neighbourManhattan = neighbourCost manhattanDist

neighbourMpTile :: BoardState -> Int
neighbourMpTile = neighbourCost misplacedTile
