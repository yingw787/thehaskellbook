-- ChapterExercises.hs
--
-- (PERSONAL NOTE: Paused for a few days and I'm a bit lost, assume copied from
-- answer key unless otherwise noted)
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
module ChapterExercises where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- Concrete type for testing.
type IIL = (Int, Int, [Int])

-- 1) Identity
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
newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldMap _ _ = mempty

instance Traversable (Constant a) where
    -- traverse _ _ = fmap C
    traverse f (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
    (=-=) = eq

main2 :: IO ()
main2 = do
    quickBatch (traversable (undefined :: Constant IIL IIL))

-- 3) Maybe
data Optional a = Nada | Yep a deriving (Eq, Ord, Show)

-- (CORRECT BY CHECKING ANSWER KEY)
instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
    foldMap f Nada = mempty
    foldMap f (Yep a) = (f a)

instance Traversable Optional where
    traverse f Nada = pure Nada
    traverse f (Yep a) = fmap Yep (f a)

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = oneof [Yep <$> arbitrary, return Nada]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq

main3 :: IO ()
main3 = do
    quickBatch (traversable (undefined :: Optional IIL))

-- 4) List
data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show)

-- From chp17/ListApplicative.hs
instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

-- (PERSONAL NOTE: Need extra help here)
instance Foldable List where
    foldMap f Nil = mempty
    foldMap f (Cons x xs) = (f x) <> (foldMap f xs)

instance Traversable List where
    traverse f Nil = pure Nil
    traverse f (Cons x xs) = Cons <$> (f x) <*> (traverse f xs)

-- mkList :: [a] -> List a
-- mkList xs = foldr Cons Nil xs

-- instance Arbitrary a => Arbitrary (List a) where
--     arbitrary = do
--         as <- arbitrary
--         return (mkList as)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        x <- arbitrary
        xs <- arbitrary
        oneof [return Nil, return (Cons x xs)]

instance Eq a => EqProp (List a) where
    (=-=) = eq

main4 :: IO ()
main4 = do
    -- let test = [(1, 2, [3]), (4, 5, [6])] :: [(Int, Int, [Int])]
    -- quickBatch $ traversable (mkList test)
    quickBatch (traversable (undefined :: List IIL))


-- 5) Three
data Three a b c = Three a b c deriving (Eq, Ord, Show)

-- (CORRECT BY CHECKING ANSWER KEY)
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = fmap (Three a b) (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

main5 :: IO ()
main5 = do
    quickBatch (traversable (undefined :: Three IIL IIL IIL))

-- 6) Pair
--
-- (CORRECT BY GHCI OUTPUT AND CHECKING ANSWER KEY)
-- (PERSONAL NOTE: Still kind of script kittying off of Three a b c)
data Pair a b = Pair a b deriving (Eq, Ord, Show)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
    foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
    traverse f (Pair a b) = fmap (Pair a) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
    (=-=) = eq

main6 :: IO ()
main6 = do
    quickBatch (traversable (undefined :: Pair IIL IIL))

-- 7) Big

-- 8) Bigger

-- 9) S

-- 10) Tree
