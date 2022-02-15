import Criterion.Main
import qualified Control.DeepSeq as DS

import Lib
-- import Samples


main :: IO ()
main = defaultMain (map benchmarks testStates)


instance DS.NFData Cell where
  rnf c = c `seq` ()

instance DS.NFData SearchState where
  rnf s = s `seq` ()


testStates :: [(BoardState, String)]
testStates = zip states names

names = ["State 1", "State 2", "State 3", "State 4", "State 5", "State 6", "State 7", "State 8", "State 9", "State 10"]

benchmarks :: (BoardState, String) -> Benchmark
benchmarks (s, name) = bgroup name $ map (($s) . run) (tail heuristics)


run :: Heuristic -> BoardState -> Benchmark
run (h, s) = bench s . nf (solve h)