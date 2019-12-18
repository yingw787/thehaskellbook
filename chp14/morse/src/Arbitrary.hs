-- Arbitrary.hs
module Arbitrary where

import Test.QuickCheck

data Trivial =
    Trivial
    deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen

main :: IO ()
main = do
    sample trivialGen

data Identity a = Identity a deriving (Eq, Show)

-- Produce random values even if the Identity structure doesn't vary.
--
-- This results in the default type `()` selected by GHCi.
identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
    a <- arbitrary
    return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

-- Generate a concrete type argument `Int` for testing with Identity data type.
identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

data Pair a b =
    Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = do
    a <- arbitrary
    b <- arbitrary
    return (Pair a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen
