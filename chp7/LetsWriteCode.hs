-- LetsWriteCode.hs
module LetsWriteCode where

tensDigit :: Integral a => a -> a
tensDigit x = d
    where   xLast = x `div` 10
            d = xLast `mod` 10

-- 1a)
tensDigit' :: Integral a => a -> a
tensDigit' x = snd ((x `div` 10) `divMod` 10)

-- 1b) YES (tensDigit' :: Integral a => a -> a)

-- 1c)
hundredsDigit' :: Integral a => a -> a
hundredsDigit' x = snd ((x `div` 100) `divMod` 10)

-- 2)
foldBool :: a -> a -> Bool -> a
foldBool x _ False = x
foldBool _ y True = y

-- Method `foldBool` as case syntax
foldBool' :: a -> a -> Bool -> a
foldBool' x y flag =
    case flag of
        True -> y
        False -> x

-- Method `foldBool` as guard syntax
foldBool'' :: a -> a -> Bool -> a
foldBool'' x y flag
    | False = x
    | otherwise = y

-- 3) (CORRECT, as checked in GHCi)
g :: (a -> b) -> (a, c) -> (b, c)
g f (a, b) = (f a, b)
