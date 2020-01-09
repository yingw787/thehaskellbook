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
data Two a b = Two a b deriving (Eq, Show)

-- (CORRECT BY CHECKING ANSWER KEY)
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

-- (INCORRECT BY QUICKBATCH CHECK)
--
-- instance (Monoid a) => Applicative (Two a) where
--     pure a = Two mempty a
--     (<*>) (Two f g) (Two a b) = Two a (g b)
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
instance Monoid a => Applicative (Two a) where
    pure a = Two mempty a
    (<*>) (Two a b) (Two c d) = Two (mappend a c) (b d)

-- (CORRECT BY CHECKING ANSWER KEY)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a' <- arbitrary
        b' <- arbitrary
        return (Two a' b')

-- (CORRECT BY CHECKING ANSWER KEY)
instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq


main2 :: IO ()
main2 = do
    let test = ("a", "b", "c")
    quickBatch $ applicative (Two test test)

-- 3)

-- 4)

-- 5)

-- 6)
