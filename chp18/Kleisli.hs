-- Kleisli.hs
--
-- Ordinary function composition:
-- `(.) :: (b -> c) -> (a -> b) -> a -> c`
module Kleisli where

import Control.Monad

-- Definition of Kleisli composition, which is a way to compose monads.
--
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
--
-- Mirrors type signature for `flip (.)`
--
-- flip (.) :: (a -> b) -> (b -> c) -> a -> c

sayHi :: String -> IO String
sayHi greeting = do
    putStrLn greeting
    getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Hello! How old are you? "


main :: IO Int
main = do
    askForAge
