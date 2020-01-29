-- Fusion.hs
module Fusion where

import Criterion.Main
import qualified Data.Vector as V


testV' :: Int -> V.Vector Int
testV' n =
    V.map (+n) $ V.map (+n) $ V.map (+n) $ V.map (+n) (V.fromList [1..10000])

-- GHC rules (wiki.haskell.org/GHC/Using_rules) will make testV the same as
-- testV'.
testV :: Int -> V.Vector Int
testV n = V.map ( (+n) . (+n) . (+n) . (+n) ) (V.fromList [1..10000])


main :: IO ()
main = defaultMain
    [   bench "vector map prefused" $
        whnf testV 9998
    ,   bench "vector map will be fused" $
        whnf testV' 9998
    ]
