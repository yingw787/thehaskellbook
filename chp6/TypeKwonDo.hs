-- TypeKwonDo.hs
module TypeKwonDo where

import Prelude

-- (1)
-- chk :: Eq b => (a -> b) -> a -> b -> Bool
-- chk = ???
--
chk :: Eq b => (a -> b) -> a -> b -> Bool
-- chk aToB a b = (==) (aToB a, b)
-- chk aToB a b = (==) (aToB a, aToB b)
--
-- (INCORRECT, compile-time error, ANSWER KEY
-- https://github.com/CarlosMChica/HaskellBook)
-- (PERSONAL NOTE: I made the mistake of using a tuple, which resulted in too
-- few arguments being applied.)
chk f a b = (==) (f a) (b)

-- (2)
-- arith :: Num b => (a -> b) -> Integer -> a -> b
-- arith = ???
-- Hint: use some arithmetic operation to combine values of type 'b'. Pick one.
arith :: Num b => (a -> b) -> Integer -> a -> b
-- arith f a b = ((f a) + 0 + b) :: Integer
-- (INCORRECT, compile-time error, ANSWER KEY
-- https://github.com/CarlosMChica/HaskellBook)
-- (PERSONAL NOTE: I'm not sure how they got here...)
arith f i a = f a * fromInteger i
