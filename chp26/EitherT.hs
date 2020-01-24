-- EitherT.hs
--
-- (Unless otherwise noted, answers from answer key:
-- https://github.com/johnchandlerburnham/hpfp)
module EitherT where


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

-- 1)
instance Functor m => Functor (EitherT e m) where
    fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

-- 2)
instance Applicative m => Applicative (EitherT e m) where
    pure x = EitherT $ pure $ pure x

    (<*>) (EitherT mefab) (EitherT mea) = EitherT $ (fmap (<*>)) mefab <*> mea

-- 3)
instance Monad m => Monad (EitherT e m) where
    return = pure
    (>>=) (EitherT ema) f = EitherT $ do
        v <- ema
        case v of
            Left e -> return (Left e)
            Right a -> runEitherT (f a)

-- 4)
swapEither :: Either e a -> Either a e
swapEither (Left e) = (Right e)
swapEither (Right a) = (Left a)

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT $ swapEither <$> ema

-- 5)
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT fac fbc (EitherT mab) = (>>=) mab (either fac fbc)
