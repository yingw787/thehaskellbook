-- MonoidExercises.hs
module MonoidExercises where

import Test.QuickCheck

import SemigroupExercises (semigroupAssoc)
import Check (monoidLeftIdentity, monoidRightIdentity)


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
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)

-- 2)

-- 3)

-- 4)

-- 5)

-- 6)

-- 7)

-- 8)
