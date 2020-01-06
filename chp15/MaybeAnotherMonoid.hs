-- MaybeAnotherMonoid.hs
module MaybeAnotherMonoid where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

import OptionalMonoid
import Check (monoidAssoc, monoidLeftIdentity, monoidRightIdentity)

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    (<>) (First' Nada) (First' Nada) = (First' Nada)
    (<>) (First' Nada) (First' (Only x)) = (First' (Only x))
    (<>) (First' (Only x)) _ = (First' (Only x))

instance Monoid (First' a) where
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
