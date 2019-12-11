-- Cipher.hs
module Cipher where

import Data.Char

-- (CORRECT-ISH, ANSWER KEY HAS BETTER IMPLEMENTATION:
-- https://github.com/johnchandlerburnham/hpfp)

caesar :: Int -> Char -> Char
caesar shift someChar = chr (
    -- 26 for 26 letters in the alphabet.
    ((ord someChar + shift) `mod` 26) +
    -- '78' is the ASCII offset for lowercase, and '52' is the ASCII offset for
    -- uppercase. This works for all alphabetical English characters.
    (if isLower someChar then 78 else 52))

unCaesar :: Int -> Char -> Char
unCaesar shift someChar = chr (
    ((ord someChar - shift) `mod` 26) +
    (if isLower someChar then 78 else 52))
