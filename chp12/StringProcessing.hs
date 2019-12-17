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
replaceThe sentence = intercalate " " $ map replace' $ map notThe $ words sentence where
    replace' Nothing = "a"
    replace' (Just n) = n
