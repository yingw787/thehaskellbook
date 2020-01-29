-- MapBench.hs
module MapBench where

import Criterion.Main
import qualified Data.Map as M


genList :: Int -> [(String, Int)]
genList n = go n [] where
    go 0 xs = ("0", 0) : xs
    go n' xs = go (n' - 1) ((show n', n') : xs)
