-- AltParsing.hs
{-# LANGUAGE QuasiQuotes #-}
module AltParsing where

import Control.Applicative
-- https://hackage.haskell.org/package/raw-strings-qq-1.1 (stack install
-- raw-strings-qq)
import Text.RawString.QQ
import Text.Trifecta


type NumberOrString = Either Integer String

eitherOr :: String
-- Requires GHC language extension
eitherOr = [r|
123
abc
456
def|]

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
-- (<|>) is a disjunction (or)
parseNos =  skipMany (oneOf "\n") >> (Left <$> integer) <|> (Right <$> some letter)

main :: IO ()
main = do
    let p f i = parseString f mempty i

    print $ p (some letter) a
    print $ p integer b

    print $ p parseNos a
    print $ p parseNos b

    print $ p (many parseNos) c
    print $ p (some parseNos) c

    print $ p (some parseNos) eitherOr
