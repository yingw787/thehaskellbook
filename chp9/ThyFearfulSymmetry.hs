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
firstSentence = "Tyger Tyger, burning bright\n"
secondSentence = "In the forests of the night\n"
thirdSentence = "What immortal hand or eye\n"
fourthSentence = "Could frame thy fearful symmetry?"
sentences = firstSentence ++ secondSentence ++ thirdSentence ++ fourthSentence

shouldEqual =
-- I think inline (=) is special for some reason; placing left bracket on
-- newline doesn't result in compile-time error, whereas placing it inline to
-- `(=)` does result in compile-time error.
    [
        "Tyger Tyger, burning bright",
        "In the forests of the night",
        "What immortal hand or eye",
        "Could frame thy fearful symmetry?"
    ]

myLines :: String -> [String]
myLines [] = []
myLines x = word : myLines rest
    where
        word = takeWhile (/= '\n') x
        rest = (drop 1) $ dropWhile (/= '\n') x

-- (3)
--
-- a' is current string.
-- b' is delimiter.
-- c' is list.
refactored :: String -> Char -> [String]
refactored someString delimiter = go someString delimiter []
    where
        go a' b' c'
            | a' == "" = reverse c'
            | (take 1 a') == " " = go
                (dropWhile (== b') a')
                b'
                (c')
            | otherwise = go
                (dropWhile (== b') $ dropWhile (/= b') a')
                b'
                (takeWhile (/= b') a' : c')

main :: IO ()
main =
    print $
        "Are they equal? "
        -- ++ show (myLines sentences == shouldEqual)
        ++ show (refactored sentences '\n' == shouldEqual)
    -- putStrLn $ show (refactored "sheryl wants fun" ' ')
