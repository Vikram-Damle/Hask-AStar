module Samples where

import Types
import Utils

-- Test Values
stateInv :: BoardState
state1   :: BoardState
state2   :: BoardState
state3   :: BoardState
state4   :: BoardState
state5   :: BoardState
state6   :: BoardState
state7   :: BoardState
state8   :: BoardState
state9   :: BoardState
state0   :: BoardState

stateInv = toBoard [4, 3, 5, 6, 7, 1, 2, 8, 0]
state1   = toBoard [1, 5, 3, 7, 0, 4, 6, 8, 2]
state2   = toBoard [4, 5, 3, 6, 7, 1, 2, 8, 0]
state3   = toBoard [4, 6, 7, 2, 0, 5, 8, 1, 3]
state4   = toBoard [3, 4, 5, 8, 1, 6, 7, 2, 0]
state5   = toBoard [4, 8, 0, 7, 3, 5, 2, 6, 1]
state6   = toBoard [8, 4, 1, 6, 3, 7, 2, 0, 5]
state7   = toBoard [6, 0, 4, 8, 3, 7, 5, 1, 2]
state8   = toBoard [6, 8, 2, 5, 0, 1, 3, 7, 4]
state9   = toBoard [4, 7, 8, 6, 5, 3, 0, 1, 2]
state0   = toBoard [7, 1, 2, 3, 8, 4, 0, 6, 5]

states :: [BoardState]
states = [
    state1,
    state2,
    state3,
    state4,
    state5,
    state6,
    state7,
    state8,
    state9,
    state0
  ]
