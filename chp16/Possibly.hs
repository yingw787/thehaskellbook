-- Possibly.hs
module Possibly where

data Possibly a = LolNop | Yeppers a deriving (Eq, Show)

-- (PERSONAL NOTE: I don't think there is an easier way to simplify it, because
-- values "LolNop" and "Yeppers" are not part of "GHC.Base".)
--
-- (CORRECT BY CHECKING ANSWER KEY)
instance Functor Possibly where
    fmap _ LolNop = LolNop
    fmap f (Yeppers a) = Yeppers (f a)
