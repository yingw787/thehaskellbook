-- Either.hs
module Either where

data Sum a b = First a | Second b deriving (Eq, Show)

-- (CORRECT BY CHECKING ANSWER KEY)
instance Functor (Sum a) where
    fmap _ (First a) = (First a)
    fmap f (Second b) = Second (f b)


-- 2)
-- A functor instance cannot apply to `First` because the inner value may be a
-- different type. For example, a failure may be stringified while the values
-- processed may be integers.
