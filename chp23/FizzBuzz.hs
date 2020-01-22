-- FizzBuzz.hs
module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State
-- http://hackage.haskell.org/package/dlist; 'stack install dlist'
import qualified Data.DList as DL


fizzBuzz :: Integer -> String
fizzBuzz n
    | mod n 15 == 0 = "FizzBuzz"
    | mod n 5 == 0 = "Buzz"
    | mod n 3 == 0 = "Fizz"
    | otherwise = show n

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

fizzbuzzList' :: [Integer] -> [String]
fizzbuzzList' list =
    let dlist = execState (mapM_ addResult' list) DL.empty
    in DL.apply dlist []

fizzbuzzList'' :: [Integer] -> DL.DList String
fizzbuzzList'' list = execState (mapM_ addResult'' list) DL.empty

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

addResult' :: Integer -> State (DL.DList String) ()
addResult' n = do
    xs <- get
    let result = fizzBuzz n
    put (DL.snoc xs result)

addResult'' :: Integer -> State (DL.DList String) ()
addResult'' n = do
    xs <- get
    let result = fizzBuzz n
    put (DL.snoc xs result)


main :: IO ()
main = do
    -- Identical output; latter collects data before dumping results to stdout,
    -- but it also reverses a list
    mapM_ (putStrLn . fizzBuzz) [1..100]
    mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]
    mapM_ putStrLn $ fizzbuzzList' [1..100]
    mapM_ putStrLn $ fizzbuzzList'' [1..100]
