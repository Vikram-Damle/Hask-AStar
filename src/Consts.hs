module Consts where

import Types (Coord)

lidx :: Coord
lidx = (0, 0)

hidx :: Coord
hidx = (2, 2)

boardSize :: Int
boardSize = (1 + snd hidx) * (1 + fst hidx)

unit1 :: Coord
unit1 = (1, 0)

unit2 :: Coord
unit2 = (0, 1)

nunit1 :: Coord
nunit1 = (-1, 0)

nunit2 :: Coord
nunit2 = (0, -1)
