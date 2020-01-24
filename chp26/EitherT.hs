-- EitherT.hs
module EitherT where


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- 1)
instance Functor m => Functor (EitherT e m) where
    fmap = undefined

-- 2)
instance Applicative m => Applicative (EitherT e m) where
    pure = undefined
    (<*>) f a = undefined

-- 3)
instance Monad m => Monad (EitherT e m) where
    return = pure
    (>>=) v f = undefined

-- 4)
swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT = undefined

-- 5)
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT = undefined
