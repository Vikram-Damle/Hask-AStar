module Utils where

import Types
    ( BoardState,
      Cell(Empty),
      Coord,
      Node(root, parent, bstate),
      SearchState(final, initial, closed) )
import Consts ( hidx, lidx, nunit1, nunit2, unit1, unit2 )
-- import System.Random
import Data.Array ( (!), (//), elems, listArray )
import Data.List (nub, find)
import Data.Set (size)
import Data.Maybe (fromJust)
import Data.Either (fromRight)


-- Setting up board

hasDup :: [Int] -> Bool
hasDup xs = nub xs /= xs

toBoard :: [Int] -> BoardState
toBoard xs = if length xs /= 9 || hasDup []
  then error $ "Invalid State: " ++ show xs
  else listArray (lidx, hidx) (map toEnum xs)


fromBoard :: BoardState -> [Int]
fromBoard = map fromEnum . elems


isSolvable :: BoardState -> Bool
isSolvable b = even flips
  where
    flips = count (elems b)
    count (x:xs)
      | x == Empty = count xs
      | otherwise = (length . filter id $ map (<x) xs) + count xs
    count [] = 0


-- Functions over coordinates

indexToCoords :: Int -> (Int, Int)
indexToCoords x = x `divMod` 3


validCoord :: Coord -> Bool
validCoord (r, c)
  | 0 > r || r > 2 = False
  | 0 > c || c > 2 = False
  | otherwise      = True


(@@) :: Coord -> Coord -> Coord
(@@) (x,y) (a, b) = (x+a, y+b)

(+@) :: Coord -> Coord -> Coord
(+@) p1 p2 = if validCoord q' then q' else p1
  where
    q' = p1 @@ p2

moveU :: Coord -> Coord
moveD :: Coord -> Coord
moveL :: Coord -> Coord
moveR :: Coord -> Coord

moveU = (+@ unit1)
moveD = (+@ nunit1)
moveL = (+@ nunit2)
moveR = (+@ unit2)


swap :: BoardState -> Coord -> Coord -> BoardState
swap brd x y = if x == y then brd else brd // [a, b]
  where
    a = (y, brd ! x)
    b = (x, brd ! y)


-- General Utility

showBoard :: BoardState -> String
showBoard b = unlines ls
  where
    b' = map (show . fromEnum) (elems b)
    b1 = take 3 b'
    b2 = take 3 $ drop 3 b'
    b3 = take 3 $ drop 6 b'
    ls = map unwords [b1, b2, b3]


fin :: BoardState
fin = toBoard [0..8]

checkFin :: Node -> Bool
checkFin n = bstate n == fin

getFin :: [Node] -> Maybe Node
getFin = find ((==fin) . bstate)

path :: SearchState -> [BoardState]
path s = map bstate (go fn [])
  where
    fn = final s
    go n ns 
      | root n    = n:ns
      | otherwise = go (parent n) (n:ns)

showSearchResults :: Either BoardState SearchState -> String
showSearchResults (Left b) = "Unsolvable Initial State:\n" ++ showBoard b
showSearchResults (Right s) = str 
  where
    b = initial s
    str1 = "Initial State:\n" ++ showBoard b
    str2 = "Final State:\n"   ++ showBoard (bstate . final $ s)
    str3 = "Number of Nodes Removed from Frontier: " ++  (show . size . closed)  s
    str4 = "Solution Length: " ++ show (length . path $ s) ++ " Nodes"
    str5 = "" -- ++ (unlines . map showBoard . path $ s)
    str6 = replicate 40 '=' 
    str  = unlines [str1, str2, str3, str4, str5, str6]

