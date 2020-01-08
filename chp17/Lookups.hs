-- Lookups.hs
--
-- Use 'pure', '(<$>)' / 'fmap', and '(<*>)' / 'apply' to make expressions
-- typecheck.
module Lookups where

import Data.List (elemIndex)


-- 1)
added :: Maybe Integer
-- added = (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 'zip [1, 2, 3] [4, 5, 6]' -> '(Num a, Num b) => [(a, b)]'
-- 'lookup 3 $ zip [1, 2, 3] [4, 5, 6]' -> 'Num a => Maybe a'
-- '(<$>) (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])' -> 'Num a => Maybe a'
--
-- (PERSONAL NOTE: Not sure how to use the above methods in order to lift the
-- value from a Maybe context)
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
-- (PERSONAL NOTE: I got it right, I forgot that the type signature constrains
-- the concrete value and I don't actually need to do any casting myself.)
added = (<$>) (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2)
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) y z

-- 3)
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y :: Maybe Int
y = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' x y

-- 4)
xs = [1, 2, 3]
ys = [4, 5, 6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum $ (,) x y
