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
