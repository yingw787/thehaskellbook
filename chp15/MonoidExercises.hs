-- MonoidExercises.hs
module MonoidExercises where

import Test.QuickCheck

import Check (monoidAssoc, monoidLeftIdentity, monoidRightIdentity)

-- 1)
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    (<>) _ _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

main1 :: IO ()
main1 = do
    quickCheck (monoidAssoc :: TrivAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)

-- 2)
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a' <- arbitrary
        return (Identity a')

instance Semigroup a => Semigroup (Identity a) where
    (<>) (Identity a) (Identity b) = Identity (a <> b)

-- (PERSONAL NOTE: Not exactly sure whether I should have divided up this into
-- Monoid and Semigroup) (Yes I should have because Semigroup must be deduced)
instance Monoid a => Monoid (Identity a) where
    mempty = (Identity mempty)
    mappend = (<>)

type IdentityAssoc = (Identity String) -> (Identity String) -> (Identity String) -> Bool

main2 :: IO ()
main2 = do
    quickCheck (monoidAssoc :: IdentityAssoc)
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)

-- 3)
--
-- (PERSONAL NOTE: Struggled a lot on this question, turns out main method
-- forgot do block, which resulted in a lot of confusion)
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a' <- arbitrary
        b' <- arbitrary
        return (Two a' b')

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (<>) (Two a1 b1) (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = (Two mempty mempty)
    mappend = (<>)

type TwoAssoc = (Two String String) -> (Two String String) -> (Two String String) -> Bool

main3 :: IO ()
main3 = do
    -- (CORRECT BY GHCI OUTPUT)
    quickCheck (monoidAssoc :: TwoAssoc)
    quickCheck (monoidLeftIdentity :: Two String String -> Bool)
    quickCheck (monoidRightIdentity :: Two String String -> Bool)

-- 4)

-- 5)

-- 6)

-- 7)

-- 8)
