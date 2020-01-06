-- SemigroupExercises.hs
module SemigroupExercises where

import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

-- 1)
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    -- (CORRECT BY GHCI OUTPUT)
    --
    -- (<>) Trivial Trivial = Trivial
    -- (<>) x Trivial = x
    -- (<>) Trivial x = x
    -- (<>) x y = x <> y
    --
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    --
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

main1 :: IO ()
main1 =
    -- (CORRECT BY GHCI OUTPUT)
    quickCheck (semigroupAssoc :: TrivAssoc)

-- 2)
newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
    -- (INCORRECT, type argument 'a' does not translate to QuickCheck
    -- generation)
    --
    -- arbitrary = return (Identity a)
    --
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    --
    arbitrary = do
        a' <- arbitrary
        return (Identity a')

instance Semigroup a => Semigroup (Identity a) where
    -- (PERSONAL NOTE: ...why does an identity newtype have a type argument...?)
    -- (<>) (Identity a) (Identity b) = (Identity a)
    --
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    -- (PERSONAL NOTE: How does this method enforce types for inner values? What
    -- if they're different?) (ANSWER: Through type signature that enforces
    -- Semigroup typeclass on inner type argument, and identical types for both
    -- 'a' and 'b'.)
    --
    (<>) (Identity a) (Identity b) = Identity (a <> b)

type IdentityAssoc = (Identity String) -> (Identity String) -> (Identity String) -> Bool

main2 :: IO ()
main2 =
    -- (CORRECT BY GHCI OUTPUT)
    quickCheck (semigroupAssoc :: IdentityAssoc)

-- 3)
data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a' <- arbitrary
        b' <- arbitrary
        return (Two a' b')

-- (PERSONAL NOTE: Not exactly sure what the book means when it says 'Ask for
-- another `Semigroup` instance)
--
-- instance Semigroup a => Semigroup b => Semigroup (Two a b) where
--     (<>) ()
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (<>) (Two a1 b1) (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

type TwoAssoc = (Two String String) -> (Two String String) -> (Two String String) -> Bool

main3 :: IO ()
main3 =
    -- (CORRECT BY GHCI OUTPUT)
    quickCheck (semigroupAssoc :: TwoAssoc)

-- 4)
data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a' <- arbitrary
        b' <- arbitrary
        c' <- arbitrary
        return (Three a' b' c')

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (<>) (Three a1 b1 c1) (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

type ThreeAssoc = (Three String String String) -> (Three String String String) -> (Three String String String) -> Bool

main4 :: IO ()
main4 =
    -- (CORRECT BY GHCI OUTPUT)
    quickCheck (semigroupAssoc :: ThreeAssoc)

-- 5)
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a' <- arbitrary
        b' <- arbitrary
        c' <- arbitrary
        d' <- arbitrary
        return (Four a' b' c' d')

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (<>) (Four a1 b1 c1 d1) (Four a2 b2 c2 d2) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

type FourAssoc = (Four String String String String) -> (Four String String String String) -> (Four String String String String) -> Bool

main5 :: IO ()
main5 =
    -- (CORRECT BY GHCI OUTPUT)
    quickCheck (semigroupAssoc :: FourAssoc)

-- 6)
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
    arbitrary = do
        oneof [return (BoolConj True), return (BoolConj False)]

instance Semigroup BoolConj where
    (<>) (BoolConj False) _ = (BoolConj False)
    (<>) _ (BoolConj False) = (BoolConj False)
    (<>) (BoolConj True) (BoolConj True) = (BoolConj True)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

main6 :: IO ()
main6 =
    -- (CORRECT BY GHCI OUTPUT)
    quickCheck (semigroupAssoc :: BoolConjAssoc)

-- 7)
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Arbitrary BoolDisj where
    arbitrary = do
        oneof [return (BoolDisj True), return (BoolDisj False)]

instance Semigroup BoolDisj where
    (<>) (BoolDisj True) _ = (BoolDisj True)
    (<>) _ (BoolDisj True) = (BoolDisj True)
    (<>) (BoolDisj False) (BoolDisj False) = (BoolDisj False)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

main7 :: IO ()
main7 =
    -- (CORRECT BY GHCI OUTPUT)
    quickCheck (semigroupAssoc :: BoolDisjAssoc)

-- 8)
--
-- (PERSONAL NOTE: Going to try to solve it by myself but need to check answer
-- key afterwards..)
--
-- (PERSONAL NOTE: Wow it actually matches, I'm pretty happy)
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a' <- arbitrary
        b' <- arbitrary
        oneof [return (Fst a'), return (Snd b')]

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    (<>) (Snd x) _ = (Snd x)
    (<>) _ (Snd x) = (Snd x)
    -- (PERSONAL NOTE: This is correct, answer key has `(<>) _ (Fst x) = (Fst
    -- x)`)
    (<>) (Fst x) (Fst x') = (Fst x')

type OrAssoc = (Or String String) -> (Or String String) -> (Or String String) -> Bool

main8 :: IO ()
main8 =
    -- (CORRECT BY GHCI OUTPUT)
    quickCheck (semigroupAssoc :: OrAssoc)

-- 9)

-- 10)

-- 11)
