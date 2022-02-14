module Types where 

import Data.Set (Set)
import Data.Array (Array, elems)

data Cell = Empty | One | Two | Three | Four | Five | Six | Seven | Eight | Nine
  deriving (Enum, Eq, Ord, Show)

type Coord = (Int, Int)

type BoardState = Array Coord Cell

data Node = Node 
  {
    sunkCost :: Int,
    heurCost :: Int,
    bstate   :: BoardState,
    parent   :: Node, 
    root     :: Bool
  }

instance Eq Node where
  n1 == n2 = compare n1 n2 == EQ


instance Ord Node where
  compare n1 n2 
    | a <  b            = LT
    | a >  b            = GT
    | a == b && h1 < h2 = LT
    | a == b && h1 > h2 = GT
    | otherwise         = EQ
    where
      a  = sunkCost n1 + heurCost n1
      b  = sunkCost n2 + heurCost n2
      h1 = hash . bstate $ n1
      h2 = hash . bstate $ n2


data SearchState = SearchState
  {
    initial  :: BoardState,
    closed   :: Set BoardState,
    open     :: Set Node,
    heurFunc :: BoardState -> Int,
    final    :: Node
  }


data Move = Up | Down | Left | Right

type CostFunc = BoardState -> Int
type Heuristic = (CostFunc, String)

hash :: BoardState -> Int
hash b = n
  where
    xs = map fromEnum . elems $ b
    n = foldl (\x y -> x*10 + y) 0 xs

