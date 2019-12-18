-- Forever.hs
module Forever where

import Control.Monad
import Data.Char
import System.Exit (exitSuccess)

-- 2)
--
-- palindrome :: IO ()
-- palindrome = forever $ do
--     line1 <- getLine
--     case (line1 == reverse line1) of
--         True -> putStrLn "It's a palindrome!"
--         False -> putStrLn "Nope!"
--
-- (INCORRECT, COMPILE-TIME ERROR)
--
-- palindrome :: IO ()
-- palindrome = forever $ do
--     line1 <- getLine
--     if (line1 == reverse line)
--     then
--         putStrLn "It's a palindrome!"
--     else
--         putStrLn "Nope!"
--         exitSuccess
--
-- (FROM ANSWER KEY)
-- (PERSONAL NOTE: Apparently in order to convert IO a0 to IO, then add another
-- do block.)
palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    if (line1 == reverse line1)
    then putStrLn "It's a palindrome!"
    else do putStrLn "Nope!"
            exitSuccess

-- 3)
palindrome' :: IO ()
palindrome' = forever $ do
    line1 <- getLine
    if (isPalindrome line1)
    then putStrLn "It's a palindrome!"
    else do putStrLn "Nope!"
            exitSuccess

isPalindrome :: String -> Bool
isPalindrome str = str' == reverse str' where
    str' = map toLower $ filter isAlpha $ str
