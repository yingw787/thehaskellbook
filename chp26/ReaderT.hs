-- ReaderT.hs
module ReaderT where


newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
    pure a = ReaderT (pure (pure a))
    (<*>) (ReaderT fmab) (ReaderT rma) = ReaderT $ (<*>) <$> fmab <*> rma

instance Monad m => Monad (ReaderT r m) where
    return = pure

    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> RedaerT r m b
    (>>=) (ReaderT rma) f = ReaderT $ \r -> do
        a <- rma r
        runReaderT (f a) r
