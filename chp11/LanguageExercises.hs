-- LanguageExercises.hs
module LanguageExercises where

import Data.Char -- `toUpper`
import Data.List -- `splitOn`

-- 1)
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

-- 2)
-- capitalizeParagraph :: String -> String
-- capitalizeParagraph paragraph =
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
capParagraph :: String -> String
-- Cast the string into a list of strings using `word`, then recursively parse
-- each sentence based on whether the last character is '.'
--
-- (THIS DOESN'T ACTUALLY APPEAR TO WORK...SO...NEED TO COME BACK TO THIS TODO)
capParagraph p = init $ concat $ map (++ " ") $ capp $ words p where
    capp  [] = []
    capp  (x:xs) = if (last x) == '.' then x:(capp' xs) else x:(capp xs)
    capp' [] = []
    capp' (x:xs) = if (last x) == '.' then (capitalizeWord x):(capp' xs) else x:(capp xs)
