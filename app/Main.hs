module Main where

import Lib
import System.IO (hPutStrLn)

main :: IO ()
main = do
  putStrLn ""
  -- # Demonstration Examples for Corner Cases
  putStrLn $ run hMispTile stateInv
  putStrLn $ run hNullHeur state1

  -- # Run all 4 heuristic searches for 3 example states 
  mapM_ rungamut (take 3 states)

  -- # Begin user interaction loop
  runLoop

  -- # Code used for measuring statistics of all 4 heuristics over 10 sample cases
  -- writeFile "outputs/stats.txt" $ "Initial States:\n\n" ++ (unlines . map showBoard $ states)
  -- let res = zipWith (\x (a, b, c) -> statsToString (x, a, b, c)) triplets (concatMap rungamut' states)
  -- appendFile "outputs/stats.txt" (unlines res)
  return ()


run :: (CostFunc, String) -> BoardState -> String
run h s = showSearchResults (solve (fst h) s) (snd h)

rungamut :: BoardState -> IO ()
rungamut s= do
  putStrLn $ run hMispTile s
  putStrLn $ run hManhDist s
  putStrLn $ run hNMspTile s
  putStrLn $ run hNManDist s


rungamut' :: BoardState -> [(String, Int, Int)]
rungamut' s = [
    uncurry getStats $ run' hMispTile s
  , uncurry getStats $ run' hManhDist s
  , uncurry getStats $ run' hNMspTile s
  , uncurry getStats $ run' hNManDist s
  ]
  where
    run' h s = (solve (fst h) s, snd h)

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


statsToString :: (Int, String, Int, Int) -> String
statsToString (a, b, c, d)  = 
     "State "    ++ show a ++ "\n"
  ++                     b ++ "\n"
  ++ "Explored " ++ show c ++ "\n"
  ++ "Length "   ++ show d ++ "\n\n"


triplets :: [Int]
triplets = concatMap (replicate 4) [1..]