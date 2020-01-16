-- LibraryFunctions.hs
module LibraryFunctions where

import Data.Foldable
import Data.Monoid


-- 1)
sum' :: (Foldable t, Num a) => t a -> a
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
sum' xs = foldr (+) 0 xs
-- Alternatively, with `foldMap`:
sum'' :: (Foldable t, Num a) => t a -> a
sum'' = getSum . foldMap Sum

-- 2)
-- (CORRECT BY GHCI OUTPUT)
product' :: (Foldable t, Num a) => t a -> a
product' xs = foldr (*) 1 xs

product'' :: (Foldable t, Num a) => t a -> a
product'' = getProduct . foldMap Product

-- 3)
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
elem' x xs = foldr ((||) . (== x)) False xs

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' x xs = getAny $ foldMap (Any . (== x)) xs

-- 4)
-- minimum :: (Foldable t, Ord a) => t a -> Maybe a
-- minimum xs = foldr ((||) . (< x)) Nothing xs
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
-- (PERSONAL NOTE: Answer key defines a monoid for helping implement minimum. I
-- would not have thought to done that.)
newtype Least a = Least { getLeast :: Maybe a } deriving (Eq, Ord, Show)

-- (PERSONAL NOTE: Have to implement my own Semigroup because of GHC upgrade.)
instance Ord a => Semigroup (Least a) where
    (<>) (Least Nothing) a = a
    (<>) a (Least Nothing) = a
    (<>) (Least (Just a)) (Least (Just b)) = Least (Just (min a b))

instance Ord a => Monoid (Least a) where
    mempty = Least Nothing
    mappend = (<>)

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' xs = getLeast $ foldMap (Least . Just) xs

-- 5)
newtype Most a = Most { getMost :: Maybe a } deriving (Eq, Ord, Show)

instance Ord a => Semigroup (Most a) where
    (<>) (Most Nothing) a = a
    (<>) a (Most Nothing) = a
    (<>) (Most (Just a)) (Most (Just b)) = Most (Just (max a b))

instance Ord a => Monoid (Most a) where
    mempty = Most Nothing
    mappend = (<>)

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' xs = getMost $ foldMap (Most . Just) xs

-- 6)

-- 7)

-- 8)

-- 9)

-- 10)


main :: IO ()
main = do
    print $ sum' [1..5]
    print $ sum'' [1..5]

    print $ product' [1..5]
    print $ product'' [1..5]

    print $ elem' 3 [1..5]
    print $ elem'' 3 [1..5]

    print $ minimum' [1..5]

    print $ maximum' [1..5]
