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

- `ReaderT`
    - See `ReaderT.hs`.

- `StateT`
    - `See `StateT.hs`.

- `ReaderT`, `WriterT`, `StateT`

```haskell
newtype Reader r a = Reader { runReader :: r -> a }

-- writers combine values monoidally
newtype Writer w a = Writer { runWriter :: (a, w) }

-- You don't need Writer or Reader (unless you want the restrictions), since
-- State can both read and write values
newtype State s a = State { runState :: s -> (a, s) }

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
```

```haskell
newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w)}
```

- Correspondence between `StateT` and `Parser`
