module Main where

import DogsRule
import Hello

main :: IO ()
main = do
  name <- getLine
  sayHello name
  dogs
