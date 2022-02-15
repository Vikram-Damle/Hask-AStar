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


-- Misplaced Tile Heuristic
-- Counts number of tiles not in their final place
-- Most Optimistic Heuristic
-- Result of relaxing the adjacency & emptyness constraint for movement 
misplacedTile :: BoardState -> Int
misplacedTile cs = ct
  where
    xs = fromBoard cs
    count = zipWith (==) xs [0..8]
    ct = length . filter (/= True) $ count


-- Manhattan Distance Heuristic
-- Sums up Manhattan Distances of all tiles from their goal positions
-- Admissible and Consistent
-- Result of relaxing the emptyness constraint for movement
manhattanDist :: BoardState -> Int
manhattanDist cs = sum dists
  where
    manDist (a, b) (c, d) = abs(a-c) + abs(b-d)
    bs = assocs (fmap (indexToCoords . fromEnum) cs)
    dists = map (uncurry manDist) bs


-- Calculate minimum heuristic costs among all neighbours
neighbourCost :: CostFunc -> BoardState -> Int
neighbourCost h = (+1) . minimum . map h . getUniqueChildren


-- Calculate minimum Manhattan cost among all neighbours
neighbourManhattan :: BoardState -> Int
neighbourManhattan = neighbourCost manhattanDist


-- Calculate minimum Misplace Tile cost among all neighbours
neighbourMpTile :: BoardState -> Int
neighbourMpTile = neighbourCost misplacedTile
