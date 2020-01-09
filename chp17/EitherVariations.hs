-- EitherVariations.hs
module EitherVariations where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- Deconflict from 'Test.QuickCheck.Success' and 'Test.QuickCheck.Failure'.
data Validation' e a = Failure' e | Success' a deriving (Eq, Show)

-- same as Either
--
-- (CORRECT BY CHECKING ANSWER KEY)
--
instance Functor (Validation' e) where
    fmap _ (Failure' e) = (Failure' e)
    fmap f (Success' a) = Success' (f a)

-- This is different
instance Monoid e => Applicative (Validation' e) where
    pure a = (Success' a)
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    --
    -- (PERSONAL NOTE: I'm not sure why this is `mappend`. It might be because
    -- it is monoidally combining the results together.)
    (<*>) (Failure' e) (Failure' r) = Failure' (e `mappend` r)
    (<*>) (Failure' e) _ = Failure' e
    (<*>) _ (Failure' a) = Failure' a
    -- (PERSONAL NOTE: I'm not sure why these *aren't* monoidally combined.)
    (<*>) (Success' a) (Success' b) = Success' (a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation' a b) where
    arbitrary = do
        a' <- arbitrary
        b' <- arbitrary
        oneof [return (Failure' a'), return (Success' b')]

-- (PERSONAL NOTE: I forgot I should define 'EqProps' for using
-- 'Test.QuickCheck.Checkers')
instance (Eq e, Eq a) => EqProp (Validation' e a) where
    (=-=) = eq

main :: IO ()
main = do
    -- (PERSONAL NOTE: I forgot how to define the input arguments here)
    --
    -- let test = Success 1
    -- quickBatch $ applicative (test :: Validation' Int Int)
    --
    -- (FROM ANSWER KEY)
    -- (PERSONAL NOTE: Remember that 'quickBatch' doesn't care about the actual
    -- value, it cares about the type, therefore providing the proper type
    -- signature is important)
    --
    let test :: Validation' (String, String, String) (String, String, String)
        test = Success' ("a", "b", "c")
    quickBatch $ applicative test
