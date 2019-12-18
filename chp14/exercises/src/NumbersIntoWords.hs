-- NumbersIntoWords.hs
module NumbersIntoWords where

-- From WordNumber.hs, pasted here because WordNumber is not part of the same
-- scope I think
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
digits n = go n []
    where   go  value list
                | value < 10 = [value] ++ list
                | otherwise = go
                    (fst $ value `divMod` 10)
                    ([snd $ value `divMod` 10] ++ list)

wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" (map (digitToWord) (digits n)))
