-- WordNumber.hs
--
-- (CORRECT, GHCI RESULTS BELOW)
--
-- *WordNumber> :r
-- [1 of 1] Compiling WordNumber       ( WordNumber.hs, interpreted )
-- Ok, one module loaded.
-- *WordNumber> wordNumber 12345
-- "one-two-three-four-five"
-- *WordNumber>

module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n
    | n == 0 = "zero"
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"
    | otherwise = error "Expecting a digit between 0 and 9 inclusive."

digits :: Int -> [Int]
-- Not exactly sure how to approach this problem, but I will see whether I can
-- recursively calculate the modulus and populate a list.
digits n = go n []
    -- NOTE: Use tabs to separate in order to successfully compile!!
    where   go  value list
                -- NOTE: Use tabs and try compiling in order to determine right
                -- number of tabs!
                | value < 10 = [value] ++ list
                | otherwise = go
                    -- NOTE: Use tab character to issue multi-line statements.
                    (fst $ value `divMod` 10)
                    ([snd $ value `divMod` 10] ++ list)

wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" (map (digitToWord) (digits n)))
