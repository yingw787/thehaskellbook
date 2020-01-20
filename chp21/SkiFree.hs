{-# LANGUAGE FlexibleContexts #-}
-- SkiFree.hs
--
-- (PERSONAL NOTE: Yeah I'm running behind already...gonna look at the answer
-- keys here...assume that all changes are from answer key:
-- https://github.com/johnchandlerburnham/hpfp)
module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data S n a = S (n a) a deriving (Eq, Show)

-- (FROM ANSWER KEY: These templates apparently make functor identity laws fail)
--
-- instance ( Functor n
--          , Arbitrary (n a)
--          , Arbitrary a )
--          => Arbitrary (S n a) where
--              arbitrary =
--                  S <$> arbitrary <*> arbitrary
--
-- instance ( Applicative n
--          , Testable (n Property)
--          , Eq a
--          , Eq (n a)
--          , EqProp a)
--          => EqProp (S n a) where
--              (=-=) = eq

instance (Eq (n a), Eq a) => EqProp (S n a) where
    (=-=) = eq

instance Functor n => Functor (S n) where
    fmap f (S x y) = S (f <$> x) (f y)

instance Foldable n => Foldable (S n) where
    foldMap f (S x y) = (foldMap f x) `mappend` (f y)

instance Traversable n => Traversable (S n) where
    traverse f (S x y) = S <$> (traverse f x) <*> (f y)

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

type IIL = (Int, Int, [Int])

main :: IO ()
main =
    -- sample' (arbitrary :: Gen (S [] Int))
    quickBatch (traversable (undefined :: S [] IIL))
