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


main :: IO ()
main = do
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    quickCheck (functorIdentity :: (Identity Int) -> Bool)
    quickCheck (functorCompose' :: IntToInt -> IntToInt -> (Identity Int) -> Bool)
