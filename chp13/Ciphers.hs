-- Ciphers.hs
module Cipher where

import Data.Char
import System.IO (BufferMode(NoBuffering), hSetBuffering, stdout)

caesar :: Int -> Char -> Char
caesar shift someChar = chr (
    ((ord someChar + shift) `mod` 26) +
    (if isLower someChar then 78 else 52))

unCaesar :: Int -> Char -> Char
unCaesar shift someChar = chr (
    ((ord someChar - shift) `mod` 26) +
    (if isLower someChar then 78 else 52))

vignere :: String -> String -> String
vignere key cleartext = map caesarHelper $ zip key' clr' where
    pre = (map toLower . filter isAlpha)
    clr' = (pre cleartext)
    key' = take (length clr') $ cycle (pre key)
    caesarHelper (a, b) = chr ((ord a + ord b - 2 * ord 'a') `mod` 26 + ord 'a')

unVignere :: String -> String -> String
unVignere key ciphertext = map caesarHelper $ zip key' ciphr' where
    pre = (map toLower . filter isAlpha)
    ciphr' = (pre ciphertext)
    key' = take (length ciphr') $ cycle (pre key)
    caesarHelper (a, b) = chr ((ord b - ord a) `mod` 26 + ord 'a')

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStr "Please input your plaintext: "
    vignerePlainText <- getLine
    putStr "Please input your keyword: "
    vignereKeyword <- getLine
    putStrLn "This is your ciphertext: "
    print $ vignere vignereKeyword vignerePlainText

    putStr "Please input your shift: "
    shift <- getLine
    putStr "Please input your character: "
    char <- getLine
    putStrLn "This is your shifted char: "
    print $ caesar (read shift :: Int) (char !! 0)
