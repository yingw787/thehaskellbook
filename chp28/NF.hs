-- NF.hs
module NF where

import Criterion.Main


myList :: [Int]
myList = [1..9999]

-- This is guarded recursion
--
-- This doesn't actually run the benchmark, makes it misleading
--
-- Replace values with bottoms in order to understand where profiler is going
--
-- Need to change whnf to nf for proper instrumenting
main :: IO ()
main = defaultMain
    [   bench "map list 9999" $
        whnf (map (+1)) myList
    ]
