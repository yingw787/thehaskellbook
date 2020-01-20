-- WarmingUp.hs
module WarmingUp where

import Data.Char


cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs


composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev


main :: IO ()
main = do
    print $ composed "Julie"
    print $ composed "Chris"

    print $ fmapped "Julie"
    print $ fmapped "Chris"
