-- Idempotence.hs
module Idempotence where

import Data.Char (toUpper)
import Data.List (sort)
import Test.QuickCheck

twice f = f . f
fourTimes = twice . twice

-- 1)
prop_IdemOne :: [Char] -> Bool
prop_IdemOne x =
    (capitalizeWord x == twice capitalizeWord x) &&
    (capitalizeWord x == fourTimes capitalizeWord x) where
        -- (PERSONAL NOTE: Not sure where method capitalizeWord came from,
        -- answer key implemented it as below:)
        capitalizeWord "" = ""
        capitalizeWord (x : xs) = toUpper x : xs

-- 2)
prop_IdemTwo :: (Ord a) => [a] -> Bool
prop_IdemTwo x =
    (sort x == twice sort x) &&
    (sort x == fourTimes sort x)

main :: IO ()
main = do
    quickCheck (prop_IdemOne :: String -> Bool)
    quickCheck (prop_IdemTwo :: String -> Bool)
