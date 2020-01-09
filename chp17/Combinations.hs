-- Combinations.hs
module Combinations where

import Control.Applicative (liftA3)


stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a ,b, c)]
combos list1 list2 list3 = liftA3 (,,) list1 list2 list3


-- (CORRECT BY GHCI OUTPUT)
main :: IO ()
main = do
    let test = "123"
    print $ combos stops vowels test
