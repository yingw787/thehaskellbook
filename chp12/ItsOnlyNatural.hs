-- ItsOnlyNatural.hs
module ItsOnlyNatural where

-- As natural as any competitive bodybuilder
data Nat = Zero | Succ Nat deriving (Eq, Show)

-- Any natural number can be represented by an integer, so no Maybe is necessary
-- here.
--
-- (CORRECT BY GHCI OUTPUT)
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

-- A natural number is a whole number (no decimal) between 0 and infinity
-- inclusive.
--
-- (CORRECT BY GHCI OUTPUT)
integerToNat :: Integer -> Maybe Nat
integerToNat x
    | x < 0 = Nothing
    | otherwise = Just (convert' x) where
        convert' int = go int Zero where
            go 0 curr = curr
            go x curr = go (x - 1) (Succ curr)
