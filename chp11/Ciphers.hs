-- Ciphers.hs
module Ciphers where

-- Needed for alphabetical checks (like method `isAlpha`:
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html)
import Data.Char

-- Write a Vigenère cipher (interwoven Caesar ciphers)
--
-- vigenère :: String -> String
-- (From https://gist.github.com/dssstr/aedbb5e9f2185f366c6d6b50fad3e4a4)
-- vigenère someStr keyword = [
--     chr(x + 65)
--     | x <- [
--         mod (someStr !! y + z !! (mod y (length key))) 26
--         -- y is the indexes of the string list
--         -- z is the ordinal conversion of the key
--         | y <-
--     ]
-- ]
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
-- (PERSONAL NOTE: Type signature should include type of the return value as
-- well.)
vignere :: String -> String -> String
vignere key cleartext = map caesarHelper $ zip key' clr' where
    -- (PERSONAL NOTE: You can define functions not needed in the outer loop in
    -- order to reduce the amount of nesting necessary for helper methods)
    --
    -- Method `pre` filters out all characters not alphabetical, then casts
    -- alphabetical characters to lowercase.
    pre = (map toLower . filter isAlpha)
    -- Value `clr'` is parsed cleartext.
    clr' = (pre cleartext)
    -- Value `key'` parses the key, cycles it (I didn't know what cycle was at
    -- this point in time, and then takes the number of characters matching
    -- length of `pre'`)
    key' = take (length clr') $ cycle (pre key)
    caesarHelper (a, b) = chr ((ord a + ord b - 2 * ord 'a') `mod` 26 + ord 'a')


-- unVigenère :: String -> String
-- unVigenère encrStr keyword = undefined
--
-- test = unVigenère . vigenère
--
unVignere :: String -> String -> String
unVignere key ciphertext = map caesarHelper $ zip key' ciphr' where
    pre = (map toLower . filter isAlpha)
    ciphr' = (pre ciphertext)
    key' = take (length ciphr') $ cycle (pre key)
    -- caesarHelper (a, b) = chr ((ord a - ord b + 2 * ord 'a') `mod` 26 + ord 'a')
    --
    -- (FROM ANSWER KEY)
    caesarHelper (a, b) = chr ((ord b - ord a) `mod` 26 + ord 'a')

main :: IO ()
main = do
    print $ vignere "ALLY" "MEET AT DAWN"
    print $ unVignere "ALLY" "MPPRAEOYWY"
