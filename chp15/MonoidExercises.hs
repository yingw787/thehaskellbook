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

-- 4)

-- 5)

-- 6)

-- 7)

-- 8)
