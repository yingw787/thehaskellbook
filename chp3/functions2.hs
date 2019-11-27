-- functions2.hs
module Functions2 where

partA :: String -> String
partA x = x ++ "!"

partB :: String -> String
partB x = [x !! 4]

partC :: String -> String
partC x = drop 9 x

main :: IO ()
main = do
    putStrLn (partA "Curry is awesome")
    putStrLn (partB "Curry is awesome!")
    putStrLn (partC "Curry is awesome!")
