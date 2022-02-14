module Change where

import Types
import Utils
import Data.Array (assocs)
import Data.Maybe (fromJust)
import Data.List (find, nub, delete)
import Data.Set (Set, notMember)


getZero :: BoardState -> Coord
getZero = fst . fromJust . find ((== Empty) . snd) . assocs


up :: BoardState -> BoardState
up b = swap b z p
  where
    z = getZero b
    p = moveU z


dn :: BoardState -> BoardState
dn b =  swap b z p
  where
    z = getZero b
    p = moveD z


lt :: BoardState -> BoardState
lt b =  swap b z p
  where
    z = getZero b
    p = moveL z


rt :: BoardState -> BoardState
rt b =  swap b z p
  where
    z = getZero b
    p = moveR z


getChildren :: BoardState -> [BoardState]
getChildren b = map ($ b) [up, dn, lt, rt]

getUniqueChildren :: BoardState -> [BoardState]
getUniqueChildren b = delete b . nub . getChildren $ b

getUnvisitedChildren :: Set BoardState -> [BoardState] -> [BoardState]
getUnvisitedChildren s = filter (`notMember` s)

getNewChildren :: Set BoardState -> BoardState -> [BoardState]
getNewChildren s = getUnvisitedChildren s . getUniqueChildren