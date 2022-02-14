module Main where

import Lib

main :: IO ()
main = do
  putStrLn ""
  -- b1 <- toBoard . map read . words <$> getLine :: IO BoardState
  putStrLn $ showSearchResults (solve misplacedTile state1)
  putStrLn $ showSearchResults (solve manhattanDist state1)
  putStrLn $ showSearchResults (solve (const 0) state1)
  putStrLn $ showSearchResults (solve (const 0) state3)
  -- putStrLn $ showSearchResults (solve misplacedTile state2)
  -- putStrLn $ showSearchResults (solve manhattanDist state2)
  return ()



-- Test Values
state1 = toBoard [1, 5, 3, 7, 0, 4, 6, 8, 2]
state2 = toBoard [4, 5, 3, 6, 7, 1, 2, 8, 0]
state3 = toBoard [4, 3, 5, 6, 7, 1, 2, 8, 0]

