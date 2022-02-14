module Samples where

import Lib

state1 :: (BoardState, String)
state2 :: (BoardState, String)
state3 :: (BoardState, String)

state1 = (toBoard [1, 5, 3, 7, 0, 4, 6, 8, 2], "State 1")
state2 = (toBoard [4, 5, 3, 6, 7, 1, 2, 8, 0], "State 2")
state3 = (toBoard [4, 3, 5, 6, 7, 1, 2, 8, 0], "State 3")
