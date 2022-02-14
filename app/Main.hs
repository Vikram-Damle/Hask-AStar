module Main where

import Lib

main :: IO ()
main = do
  putStrLn ""
  putStrLn $ run hNullHeur state1
  putStrLn $ run hMispTile state1
  putStrLn $ run hManhDist state1
  putStrLn $ run hNMspTile state1
  putStrLn $ run hNManDist state1

  putStrLn $ run hMispTile state2
  putStrLn $ run hManhDist state2
  putStrLn $ run hNMspTile state2
  putStrLn $ run hNManDist state2

  putStrLn $ run hMispTile state3

  runLoop
  return ()


run h s = showSearchResults (solve (fst h) s) (snd h)

-- Test Values
state1 = toBoard [1, 5, 3, 7, 0, 4, 6, 8, 2]
state2 = toBoard [4, 5, 3, 6, 7, 1, 2, 8, 0]
state3 = toBoard [4, 3, 5, 6, 7, 1, 2, 8, 0]



runLoop :: IO ()
runLoop = do
  showInstr
  t <- readLn :: IO Int
  if t == 0
    then return ()
    else do
      i <- getInput
      putStrLn $ uncurry run i
      runLoop