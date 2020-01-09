-- Instances.hs
module Instances where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- 1)
data Pair a = Pair a a deriving (Eq, Show)

-- (PERSONAL NOTE: Instances of Applicatives...? Not quite sure what this
-- means...)
instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    pure a = Pair a a
    (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a' <- arbitrary
        return (Pair a' a')

instance (Eq a) => EqProp (Pair a) where
    (=-=) = eq


main1 :: IO()
main1 = do
    -- let test :: Pair Int Int
    --     test = Pair 1 1
    -- quickBatch $ (applicative test)
    --
    let test = ("a", "b", "c")
    quickBatch $ applicative (Pair test test)

-- 2)

-- 3)

-- 4)

-- 5)

-- 6)
