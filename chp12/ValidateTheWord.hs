-- ValidateTheWord.hs
module ValidateTheWord where

import Data.Char

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aeiou"

-- (CORRECT BY GHCI OUTPUT)
mkWord :: String -> Maybe Word'
mkWord sentence = count'
        $ map toLower
        $ filter isAlpha sentence where
    count' sentence = go sentence 0 0 where
        go "" nv nc = if nv > nc
                        then Nothing
                        else Just (Word' sentence)
        go (x:xs) nv nc = if (elem x vowels)
                            then go xs (nv + 1) nc
                            else go xs nv (nc + 1)
