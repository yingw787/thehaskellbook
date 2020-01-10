-- EitherMonad2.hs
module EitherMonad2 where


data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap = undefined

instance Applicative (Sum a) where
    pure = undefined
    (<*>) = undefined

instance Monad (Sum a) where
    return = pure
    (>>=) = undefined
