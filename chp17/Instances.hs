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
--
-- (CORRECT BY GHCI OUTPUT)
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure a = Three mempty mempty a
    (<*>) (Three a b c) (Three d e f) = Three (mappend a d) (mappend b e) (c f)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a' <- arbitrary
        b' <- arbitrary
        c' <- arbitrary
        return (Three a' b' c')

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq


main3 :: IO ()
main3 = do
    let test = ("a", "b", "c")
    quickBatch $ applicative (Three test test test)

-- 4)
--
-- (CORRECT BY GHCI OUTPUT)
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative (Three' a) where
    pure a = Three' mempty a a
    (<*>) (Three' a b b') (Three' c d d') = Three' (mappend a c) (b d) (b' d')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        return (Three' a b b')

instance (Eq a, Eq b) => EqProp (Three' a b) where
    (=-=) = eq


main4 :: IO ()
main4 = do
    let test = ("a", "b", "c")
    quickBatch $ applicative (Three' test test test)

-- 5)

-- 6)
