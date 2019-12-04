{-# LANGUAGE NoMonomorphismRestriction #-}
-- MatchTheTypes.hs
module MatchTheTypes where

-- import Data.List

-- (1)
-- i :: Num a => a
-- i = 1
--
-- i :: a
-- i = 1

-- (2)
-- f :: Float
-- f = 1.0
--
-- f :: Num a => a
-- f = 1.0

-- (3)
-- f :: Float
-- f = 1.0
--
-- f :: Fractional a => a
-- f = 1.0

-- (4)
-- f :: Float
-- f = 1.0
--
-- f :: RealFrac a => a
-- f = 1.0

-- (5)
-- freud :: a -> a
-- freud x = x
--
-- freud :: Ord a => a -> a
-- freud x = x

-- (6)
-- freud' :: a -> a
-- freud' x = x
--
-- freud' :: Int -> Int
-- freud' x = x

-- (7)
-- myX = 1 :: Int
-- sigmund :: Int -> Int
-- sigmund x = myX
--
-- myX = 1 :: Int
-- sigmund :: a -> a
-- sigmund x = myX

-- (8)
-- myX = 1 :: Int
-- sigmund' :: Int -> Int
-- sigmund' x = myX
--
-- myX = 1 :: Int
-- sigmund' :: Num a => a -> a
-- sigmund' x = myX

-- (9)
-- jung :: Ord a => [a] -> a
-- jung xs = head (sort xs)
--
-- jung :: [Int] -> Int
-- jung xs = head (sort xs)

-- (10)
-- young :: [Char] -> Char
-- young xs = head (sort xs)
--
-- young :: Ord a => [a] -> a
-- young xs = head (sort xs)

-- (11)
-- mySort :: [Char] -> [Char]
-- mySort = sort
-- signifier :: [Char] -> Char
-- signifier xs = head (mySort xs)
--
-- mySort :: [Char] -> [Char]
-- mySort = sort
-- signifier :: Ord a => [a] -> a
-- signifier xs = head (mySort xs)
