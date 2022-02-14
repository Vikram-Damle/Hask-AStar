import Criterion.Main
import qualified Control.DeepSeq as DS

import Lib
import Samples


main :: IO ()
main = defaultMain (map benchmarks testStates)


instance DS.NFData Cell where
  rnf c = c `seq` ()

instance DS.NFData SearchState where
  rnf s = s `seq` ()


testStates :: [(BoardState, String)]
testStates = [state1, state2, state3]

benchmarks :: (BoardState, String) -> Benchmark
benchmarks (s, name) = bgroup name [
        bench "Misplaced Tile Heuristic" $ nf (solve misplacedTile) s,
        bench "Manhattan Dist Heuristic" $ nf (solve manhattanDist) s
        -- bench "State 3" $ nf (solve misplacedTile) state3
      ]