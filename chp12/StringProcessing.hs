-- StringProcessing.hs
module StringProcessing where

import Data.List

-- 1)
--
-- (CORRECT BY GHCI OUTPUT)
notThe :: String -> Maybe String
notThe a
    | a == "the" = Nothing
    | otherwise = Just a

-- (CORRECT BY GHCI OUTPUT)
replaceThe :: String -> String
replaceThe sentence = intercalate " "
        $ map replace'
        $ map notThe
        $ words sentence where
    replace' Nothing = "a"
    replace' (Just n) = n

-- 2)
--
-- Recursively count the number of instances of Nothing with Just n by
-- incrementing a counter, and add 0 otherwise.
--
-- (I'm not sure how to maintain state between different elements in a list...)
--
-- countTheBeforeVowel :: String -> Integer
-- countTheBeforeVowel = count' $ map notThe $ words sentence where
--     count' (Nothing : Just n) = 1
--     count' _ = 0
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
isVowel :: Char -> Bool
isVowel c = (elem c "aeiouAEIOU")

countTheBeforeVowel :: String -> Integer
-- Use a `go` method in order to keep a counter.
-- Use nested pattern matching in order to fetch the first character of a string
-- in a sequence of strings.
countTheBeforeVowel str = go (words str) 0 where
    go [] n = n
    go [x] n = n
    go (x:(c:cs):xss) n = if (x == "the" && (isVowel c))
                            then go xss (n + 1)
                            else go xss n
