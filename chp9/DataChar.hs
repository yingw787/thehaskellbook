-- DataChar.hs
module DataChar where

import Data.Char

-- 2)
-- Method names MUST start with lowercase, otherwise will be construed by
-- compiler as a data constructor.
--
-- Prelude> :l DataChar.hs
-- [1 of 1] Compiling DataChar         ( DataChar.hs, interpreted )
-- Ok, one module loaded.
-- *DataChar> two "HbEfLrLxO"
-- "HELLO"
-- *DataChar>
--
two :: [Char] -> [Char]
two x = filter isUpper x

-- 3)
--
-- (INCORRECT)
--
-- capitalizeFirstLetter :: [Char] -> [Char]
-- capitalizeFirstLetter (x : xs) = (toUpper (x !! 0)) : [] ++ xs
-- capitalizeFirstLetter _ = ""
--
-- FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp
-- (PERSONAL NOTE: I think I overthought it, `cons` operator apparently can
-- accept Char to [Char].)
--
capFirst :: String -> String
capFirst (x : xs) = (toUpper x) : xs
capFirst _ = ""

-- 4)
--
-- (CORRECT)
--
-- *DataChar> holler "woot"
-- "WOOT"
-- *DataChar>
--
holler :: String -> String
holler "" = ""
holler (x : xs) = (toUpper x) : holler xs

-- 5)
--
-- (CORRECT)
-- (PERSONAL NOTE: Haskell apparently doesn't have an empty character. The
-- closest is '\x00' or the null character. Therefore, default to lists, since
-- we haven't really reached the section on Maybe yet. See:
-- https://stackoverflow.com/a/58924575)
capFirstOnly :: String -> String
capFirstOnly "" = ""
capFirstOnly someStr = toUpper (head someStr) : ""

-- 6)
--
-- (INCORRECT, ANSWER KEY BELOW: https://github.com/johnchandlerburnham/hpfp)
--
-- capFirstOnlyComposed :: (String -> Char) -> String -> String
-- capFirstOnlyComposed _ "" = ""
-- capFirstOnlyComposed f someStr = f someStr : ""
--
-- Pointfree notation.
--
headToUpper :: String -> Char
headToUpper = toUpper . head
