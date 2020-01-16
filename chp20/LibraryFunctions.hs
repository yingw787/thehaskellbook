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

-- 5)

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
