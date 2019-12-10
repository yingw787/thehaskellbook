-- ThyFearfulSymmetry.hs
module ThyFearfulSymmetry where

-- (1)
--
-- Solution:
-- myWords :: [Char] -> [[Char]]
myWords someString = go someString []
    where
        go currentString currentListOfStrings
            | currentString == "" = reverse currentListOfStrings
            | currentString == " " = reverse currentListOfStrings
            | otherwise = go
                (dropWhile (== ' ') $ dropWhile (/= ' ') currentString)
                (takeWhile (/= ' ') currentString : currentListOfStrings)

-- (2)

-- (3)
