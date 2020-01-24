# Chapter 26

- Monad transformers
    - Get comfortable with stacking monad transformers

- `MaybeT`

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance Applicative m => Applicative (MaybeT m) where
    pure x = MaybeT (pure (pure x))

    (<*>) (MaybeT fab) (MaybeT mma) = MaybeT $ (<*>) <$> fab <*> mma

instance Monad m => Monad (MaybeT m) where
    return = pure

    (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (>>=) (MaybeT ma) f = MaybeT $ do
        v <- ma
        case v of
            Nothing -> return Nothing
            Just y = runMaybeT (f y)
```

- `EitherT`
    - See `EitherT.hs`.
