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
            | (take 1 currentString) == " " = go
                (dropWhile (== ' ') currentString)
                (currentListOfStrings)
            | otherwise = go
                (dropWhile (== ' ') $ dropWhile (/= ' ') currentString)
                (takeWhile (/= ' ') currentString : currentListOfStrings)

-- ANSWER KEY: https://github.com/johnchandlerburnham/hpfp
--
-- (PERSONAL NOTE: This works for the simple solution, but does not effectively
-- trim spaces from the beginning or the end of the string.)
--
-- split :: String -> [String]
-- split [] = []
-- split x = word : split rest
--     where
--         word = takeWhile (/= ' ') x
--         rest = (drop 1) $ dropWhile (/= ' ') x

-- (2)

-- (3)
