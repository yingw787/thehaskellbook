-- Arith4.hs
module Arith4 where

-- 4)
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do
    print (roundTrip 4)
    print (id 4)

-- Evaluation results convert to String, and then back to original type, as
-- enforced by the type signature.

-- 5)
--
-- (CORRECT, ANSWER KEY https://github.com/johnchandlerburnham/hpfp)
roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

-- 6)
--
-- (INCORRECT, need to cast input arguments using `(::)`.)
roundTrip' :: (Show a, Read b) => a -> b
roundTrip' a = read . show
