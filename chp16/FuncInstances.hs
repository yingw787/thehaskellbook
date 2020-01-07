-- FuncInstances.hs
module FuncInstances where

import Test.QuickCheck
-- For `Data.Fun`
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b)-> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
type IntToInt = Fun Int Int

-- Usage for quickCheck functions.
functorCompose' :: (Eq (f c), Functor f) => Fun a b -> Fun b c -> f a -> Bool
functorCompose' (Fun _ f) (Fun _ g) x = (fmap g (fmap f x)) == (fmap (g . f) x)


-- 1)
newtype Identity a = Identity a deriving (Eq, Show)
-- (PERSONAL NOTE: Not sure how to start off..)
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
    a' <- arbitrary
    return (Identity a')

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

-- 2)
data Pair a = Pair a a deriving (Eq, Show)

-- (PERSONAL NOTE: Just because type constructor for Pair has the same type
-- arguments, doesn't mean that arbitrary declaration can have the same
-- reference (since it is a data constructor)).
--
-- (INCORRECT, GHC COMPILE ERROR)
--
-- instance Arbitrary a => Arbitrary (Pair a b) where
--     arbitrary = do
--         a' <- arbitrary
--         b' <- arbitrary
--         return (Pair a' b')
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        a <- arbitrary
        a' <- arbitrary
        return (Pair a a')

-- (INCORRECT, GHC COMPILE ERROR)
--
-- instance Functor Pair where
--     fmap f (Pair a a) = Pair (f a) (f a)
--
-- (CORRECT BY GHCI OUTPUT AND CHECKING ANSWER KEY)
--
instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

-- 3)
data Two a b = Two a b deriving (Eq, Show)

-- (PERSONAL NOTE: Not sure whether there should be two arbitrary declarations,
-- one holding 'a' constant and one holding 'b' constant, in order to ensure
-- kind-ness of arbitrary functor check) (I'm not sure how multiple functor
-- declarations would work though; how would you know which one to choose? Tuple
-- just passes the first argument on through instead of having multiple
-- functors, so I think it should be fine)
--
-- (CORRECT BY GHCI OUTPUT AND CHECKING ANSWER KEY)
--
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a' <- arbitrary
        b' <- arbitrary
        return (Two a' b')

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

-- 4)

-- 5)

-- 6)

-- 7)

-- 8)


main :: IO ()
main = do
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    quickCheck (functorIdentity :: (Identity Int) -> Bool)
    quickCheck (functorCompose' :: IntToInt -> IntToInt -> (Identity Int) -> Bool)

    -- (PERSONAL NOTE: Not 'Pair Int Int', because it would not have the proper
    -- kind-ness for a functor).
    quickCheck (functorIdentity :: (Pair Int) -> Bool)
    quickCheck (functorCompose' :: IntToInt -> IntToInt -> (Pair Int) -> Bool)

    quickCheck (functorIdentity :: (Two Int Int) -> Bool)
    quickCheck (functorCompose' :: IntToInt -> IntToInt -> (Two Int Int) -> Bool)
