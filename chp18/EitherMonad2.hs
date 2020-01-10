-- EitherMonad2.hs
module EitherMonad2 where


data Sum a b = First a | Second b deriving (Eq, Show)

-- (CORRECT BY CHECKING ANSWER KEY)
instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    -- (MAY NOT BE CORRECT)
    --
    -- pure = Second
    --
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    --
    pure b = Second b
    --
    -- (CORRECT BY CHECKING ANSWER KEY)
    --
    (First a) <*> _ = First a
    _ <*> (First a) = First a
    (Second a) <*> (Second b) = Second (a b)

instance Monad (Sum a) where
    return = pure
    --
    -- (PERSONAL NOTE: Not sure how to start with this...)
    --
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    -- (PERSONAL NOTE: Wait, 'bind' removes monadic structure?)
    --
    (>>=) (Second b) f = (f b)
