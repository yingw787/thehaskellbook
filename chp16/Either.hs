-- Either.hs
module Either where

data Sum a b = First a | Second b deriving (Eq, Show)

-- (CORRECT BY CHECKING ANSWER KEY)
instance Functor (Sum a) where
    fmap _ (First a) = (First a)
    fmap f (Second b) = Second (f b)
