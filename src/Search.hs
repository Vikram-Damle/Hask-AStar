module Search where

import Types
    ( BoardState,
      CostFunc,
      Node(Node, sunkCost, bstate),
      SearchState(SearchState, heurFunc, final, open, closed) )
import Utils ( isSolvable, getFin )
import Change ( getNewChildren ) 

import Control.Monad.State.Lazy
    ( MonadState(put, get), execState, State )
import Data.Set (findMin, insert, delete, singleton, empty)
import Data.List (foldl')

makechild :: CostFunc -> Node -> BoardState -> Node
makechild h b cboard = Node csc chc cboard cpar croot
  where
    bboard = bstate b
    csc = 1 + sunkCost b
    chc = h cboard
    cpar = b
    croot = False


step :: State SearchState Node
step = do
  s <- get
  let op = open s
  let cl = closed s
  let b = findMin op
  let h = heurFunc s
  let cs = getNewChildren cl . bstate $ b
  let ns = map (makechild h b) cs
  case getFin ns of
    Just n  -> do
      put s {final = n}
      return n
    Nothing -> do
      let op' = foldl' (flip insert) (delete b op) ns
      let cl' = insert (bstate b) cl
      put s {open = op', closed = cl'}
      step


constructInitial :: CostFunc -> BoardState -> SearchState
constructInitial heur b = s
  where
    scost = 0
    hcost = heur b
    n = Node 0 hcost b n True
    s = SearchState b empty (singleton n) heur n

solve :: CostFunc -> BoardState -> Either BoardState SearchState
solve h b = if isSolvable b 
  then Right (execState step . constructInitial h $ b)
  else Left b


  