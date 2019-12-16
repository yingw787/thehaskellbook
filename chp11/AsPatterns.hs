-- AsPatterns.hs
module AsPatterns where

import Data.Char

-- as-pattern: pattern match on something and still refer to the entire original
-- value.
--
-- `@` introduces a binding called `t` to refer to the entire tuple than just a
-- part.
f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
    print a
    return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

-- 1)
--
-- ANSWER KEY HAS DIFFERENT SOLUTION
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf a b = b' == a where
    b' = take (length a) $ filter (`elem` a) b

-- Not sure how to use as-patterns for this question
-- (ANSWER KEY DOESNT USE AS-PATTERNS FOR THIS PROBLEM EITHER)

-- 2)
--
-- (CORRECT BY GHCI OUTPUT)
-- (PERSONAL NOTE: Pretty satisfied that I could solve this without looking at
-- the answer key, and answer key pretty much matched this)
capitalizeWords :: String -> [(String, String)]
capitalizeWords sentence = map capitalize' $ words sentence where
    capitalize' xs@(x:y) = (xs, toUpper x : y)
