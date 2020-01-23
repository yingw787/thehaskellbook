-- Fractions.hs
{-# LANGUAGE OverloadedStrings #-}

-- Parsing fractions from text
module Text.Fractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta


badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    return (numerator % denominator)


main :: IO ()
main = do
    let parseFraction' = parseString parseFraction mempty

    print $ parseFraction' shouldWork
    print $ parseFraction' shouldAlsoWork
    print $ parseFraction' alsoBad
    print $ parseFraction' badFraction
