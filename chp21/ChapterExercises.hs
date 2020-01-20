-- ChapterExercises.hs
module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- Concrete type for testing.
type IIL = (Int, Int, [Int])

-- 1) Identity
--
-- (PERSONAL NOTE: Looking at answer key to figure out how to get started)
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
    foldMap f (Identity a) = (f a)

instance Traversable Identity where
    traverse f (Identity a) = fmap Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq


main1 :: IO ()
main1 = do
    quickBatch (traversable (undefined :: Identity IIL))

-- 2) Constant

-- 3) Maybe

-- 4) List

-- 5) Three

-- 6) Pair

-- 7) Big

-- 8) Bigger

-- 9) S

-- 10) Tree
