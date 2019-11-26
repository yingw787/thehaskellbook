-- print3fixed.hs
-- From print3broken.hs
--
-- In order to fix print3fixed.hs, needed to refer to answer key here:
-- https://github.com/OCExercise/haskellbook-solutions/blob/master/chapters/chapter03/README.md
--
-- Two alternatives: Copy string variable, or increase the scope of variable
-- "greeting".
module Print3Fixed where

greeting = "Yarrrrr"

printSecond :: IO()
printSecond = do
    putStrLn greeting

main :: IO ()
main = do
    putStrLn greeting
    printSecond
