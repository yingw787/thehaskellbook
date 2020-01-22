-- ReaderPractice.hs
--
-- (PERSONAL NOTE: Leaning on answer key for solutions...) (Unless otherwise
-- specified, all answers are from answer key:
-- https://github.com/johnchandlerburnham/hpfp)
--
module ReaderPractice where

import Control.Applicative
import Data.Maybe


x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- (PERSONAL NOTE: `lookup` is a method in core GHC ('GHC.List'))
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
-- (PERSONAL NOTE: I could have figured this out myself lol)
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
-- (CORRECT BY CHECKING ANSWER KEY)
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
-- (CORRECT BY CHECKING ANSWER KEY)
z' n = lookup n $ zip x z

-- Makes a tuple of xs and ys
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

-- Makes a tuple of ys and zs
x2 :: Maybe (Integer, Integer)
x2 = (,) <$> ys <*> zs

-- Takes one input, makes a tuple of results of two applications of z'
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (,) zn zn where
    zn = z' n
