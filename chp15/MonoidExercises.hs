-- MonoidExercises.hs
module MonoidExercises where

import Test.QuickCheck
import Test.QuickCheck.Gen.Unsafe (promote)

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
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
    arbitrary = do
        oneof [return (BoolConj True), return (BoolConj False)]

instance Semigroup BoolConj where
    (<>) (BoolConj True) (BoolConj True) = (BoolConj True)
    (<>) _ _ = (BoolConj False)

instance Monoid BoolConj where
    mempty = (BoolConj True)
    mappend = (<>)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

main4 :: IO ()
main4 = do
    -- (CORRECT BY GHCI OUTPUT)
    quickCheck (monoidAssoc :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)

-- 5)
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
    arbitrary = do
        oneof [return (BoolDisj True), return (BoolDisj False)]

instance Semigroup BoolDisj where
    (<>) (BoolDisj False) (BoolDisj False) = (BoolDisj False)
    (<>) _ _ = (BoolDisj True)

instance Monoid BoolDisj where
    mempty = (BoolDisj False)
    mappend = (<>)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

main5 :: IO ()
main5 = do
    -- (CORRECT BY GHCI OUTPUT)
    quickCheck (monoidAssoc :: BoolDisjAssoc)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

-- 6)
newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Show a, Show b) => Show (Combine a b) where
    show _ = "Combine"

instance (Semigroup b) => Semigroup (Combine a b) where
    (<>) (Combine f) (Combine g) = Combine (\n -> (f n) <> (g n))

instance (Monoid b) => Monoid (Combine a b) where
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    mempty = Combine (\n -> mempty)
    mappend = (<>)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = fmap Combine $ promote (\n -> coarbitrary n arbitrary)

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
combineLeftIdentity :: (Combine String String) -> String -> Bool
combineLeftIdentity a s = ((unCombine (mempty <> a)) s) == ((unCombine a) s)

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
combineRightIdentity :: (Combine String String) -> String -> Bool
combineRightIdentity a s = ((unCombine (a <> mempty)) s) == ((unCombine a) s)

main9 :: IO ()
main9 = do
    quickCheck combineLeftIdentity
    quickCheck combineRightIdentity

-- 7)

-- 8)
