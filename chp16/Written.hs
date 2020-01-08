{-# LANGUAGE FlexibleInstances #-}
-- Written.hs
--
-- No answer key for this section, rely on Test.QuickCheck for correctness.
module Written where

import Test.QuickCheck
-- For 'Data.Fun'.
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
--
-- (CORRECT BY GHCI OUTPUT)
data Quant a b = Finance | Desk a | Floor b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary = do
        a' <- arbitrary
        b' <- arbitrary
        oneof [return $ Finance, return $ Desk a', return $ Floor b']

-- Parentheses are important in order to avoid 'fmap has different numbers of
-- arguments' error.
instance Functor (Quant e) where
    fmap _ Finance = Finance
    fmap _ (Desk a) = Desk a
    fmap f (Floor b) = Floor (f b)

-- 2)
--
-- (CORRECT BY GHCI OUTPUT)
data K a b = K a deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
    arbitrary = do
        a' <- arbitrary
        return (K a')

-- Yeah, it isn't interesting by itself...lifted type argument is elided.
instance Functor (K e) where
    fmap f (K a) = K a

-- 3)
--
-- (CORRECT BY GHCI OUTPUT)
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Flip K' a b) where
    arbitrary = do
        a' <- arbitrary
        return (Flip (K' a'))

-- (PERSONAL NOTE: I have no idea what I'm doing)
instance Functor (Flip K' a) where
    fmap f (Flip (K' a)) = fmap Flip K' (f a)

-- 4)
data EvilGoateeConst a b = GoatyConst b

-- 5)
data LiftItOut f a = LiftItout (f a)

-- 6)
data Parappa f g a = DaWrappa (f a) (g a)

-- 7)
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

-- 8)
data Notorious g o a t = Notorious (g o) (g a) (g t)

-- 9)
data List a = Nil | Cons a (List a)

-- 10)
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

-- 11)
data TalkToMe a = Halt | Print String a | Read (String -> a)


main :: IO ()
main = do
    quickCheck (functorIdentity :: (Quant Int Int) -> Bool)
    quickCheck (functorCompose' :: IntToInt -> IntToInt -> (Quant Int Int) -> Bool)

    quickCheck (functorIdentity :: (K Int Int) -> Bool)
    quickCheck (functorCompose' :: IntToInt -> IntToInt -> (K Int Int) -> Bool)

    quickCheck (functorIdentity :: (Flip K' Int Int) -> Bool)
    quickCheck (functorCompose' :: IntToInt -> IntToInt -> (Flip K' Int Int) -> Bool)
