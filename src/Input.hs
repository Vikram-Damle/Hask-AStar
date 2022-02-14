module Input where

import Types
import Utils
import Heuristics

showInstr :: IO ()
showInstr = putStrLn instructions
  where
    instructions = "Enter 0 to exit program\nEnter 1 to solve a custom problem\n"


boardInstructions :: IO ()
boardInstructions = putStrLn "Enter numbers 0-8 space separated. 0 represents the blank tile. The goal state is assumed to be 0 1 2 3 4 5 6 7 8\n"

takeBoardInput :: IO BoardState
takeBoardInput = do
  b <- toBoard . map read . words <$> getLine
  -- b <- words <$> readLn
  if isValid b
    then return b
    else do
      putStrLn "Invalid Board Configuration\nPlease enter the configuration again:\n"
      takeBoardInput


heurInstructions :: IO ()
heurInstructions = putStrLn $ "Select Heuristic\n" ++ unlines heurs ++ "\n(Enter appropriate digit)\n"
  where
    heurs = [show i ++ ":  " ++ snd h | (i, h) <- zip [1..] heuristics]

takeHeurInput :: IO Heuristic
takeHeurInput = do
  n <- readLn :: IO Int
  if n > length heuristics || n <= 0
    then do
      putStrLn "Invalid Heuristic Index"
      takeHeurInput
    else 
      return (heuristics !! (n-1))


getInput :: IO (Heuristic, BoardState)
getInput = do
  boardInstructions
  b <- takeBoardInput
  heurInstructions
  h <- takeHeurInput
  return (h, b)
  
