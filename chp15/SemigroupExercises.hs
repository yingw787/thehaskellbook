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
    quickCheck (semigroupAssoc :: IdentityAssoc)
