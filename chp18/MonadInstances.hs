-- MonadInstances.hs
--
-- (PERSONAL NOTE: It's been two days since I touched Haskell and monads had
-- been slippery, so I'm not sure how much of this I can do by myself..)
module MonadInstances where

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


type III = (Int, Int, Int)


-- 1)
data Nope a = NopeDotJpg deriving (Eq, Show)

-- (CORRECT BY CHECKING ANSWER KEY)
instance Functor Nope where
    fmap _ NopeDotJpg = NopeDotJpg

-- (PARTIALLY CORRECT BY CHECKING ANSWER KEY, `pure` should pattern match with
-- empty argument.)
instance Applicative Nope where
    pure _ = NopeDotJpg
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    return = pure
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    (>>=) _ _ = NopeDotJpg

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
instance Eq a => EqProp (Nope a) where
    (=-=) = eq

main1 :: IO ()
main1 = do
    quickBatch $ functor (NopeDotJpg :: Nope III)
    quickBatch $ applicative (NopeDotJpg :: Nope III)
    quickBatch $ monad (NopeDotJpg :: Nope III)

-- 2)
data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
-- (PERSONAL NOTE: It seems like "BahEither" type arguments determine whether
-- Left or Right gets proper lifted.)
instance Functor (BahEither b) where
    fmap f (PLeft a) = PLeft (f a)
    fmap _ (PRight b) = PRight b

-- (MOSTLY CORRECT BY CHECKING ANSWER KEY)
instance Applicative (BahEither b) where
    pure a = PLeft a
    (<*>) (PLeft a) (PLeft b) = PLeft (a b)
    (<*>) (PRight a) _ = PRight a
    (<*>) _ (PRight a) = PRight a
    -- (PERSONAL NOTE: Don't actually need this line, and I'm not quite sure
    -- what it does anyways.)
    --
    -- (<*>) (PRight a) (PRight b) = PRight (a `mappend` b)

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
instance Monad (BahEither a) where
    return = pure
    (>>=) (PLeft a) f = (f a)
    (>>=) (PRight b) f = PRight b

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither a b) where
    arbitrary = do
        a' <- arbitrary
        b' <- arbitrary
        oneof [return (PLeft a'), return (PRight b')]

instance (Eq a, Eq b) => EqProp (BahEither a b) where
    (=-=) = eq

main2 :: IO ()
main2 = do
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    -- (PERSONAL NOTE: Did not know that you could pass "bottom" into
    -- `quickBatch`.)
    quickBatch $ functor (undefined :: BahEither III III)
    quickBatch $ applicative (undefined :: BahEither III III)
    quickBatch $ monad (undefined :: BahEither III III)

-- 3)
--
-- (CORRECT BY GHCI OUTPUT)
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure a = Identity a
    (<*>) (Identity a) (Identity b) = Identity (a b)

instance Monad Identity where
    return = pure
    (>>=) (Identity a) f = f a

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a' <- arbitrary
        return (Identity a')

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

main3 :: IO ()
main3 = do
    quickBatch $ functor (undefined :: Identity III)
    quickBatch $ applicative (undefined :: Identity III)
    quickBatch $ monad (undefined :: Identity III)

-- 4)
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    fmap f Nil = Nil
    fmap f (Cons a as) = Cons (f a) (fmap f as)

-- (FROM chp17/ListApplicative.hs)
append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

-- (FROM chp17/ListApplicative.hs)
instance Applicative List where
    pure a = Cons a Nil
    (<*>) _ Nil = Nil
    (<*>) Nil _ = Nil
    -- (PERSONAL NOTE: Still not sure what this line means..)
    (<*>) (Cons f fs) as = (fmap f as) `append` ((<*>) fs as)

mkList :: [a] -> List a
mkList xs = foldr Cons Nil xs

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        as <- arbitrary
        return (mkList as)

instance Eq a => EqProp (List a) where
    (=-=) = eq

main4 :: IO ()
main4 = do
    let test = [(1, 2, 3), (4, 5, 6)] :: [(Int, Int, Int)]
    quickBatch $ functor (mkList test)
    quickBatch $ applicative (mkList test)
