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

```haskell
type Parser a = String -> Maybe (a, String)

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

type Parser = StateT String Maybe
```

- Types you probably don't want to use
    - ListT and Writer/WriterT shouldn't be used

- Why not `Writer`/`WriterT`?
    - Could be too lazy or too strict
    - Memory usage issues
    - Can't retrieve intermediate state (e.g. log output during daemon process)

- The `ListT` you want isn't made from the `List` type
    - Use `pipes` or `conduit`
    - Look at `AmbT` by Conal Elliot (look at `ContT` and `Amb`)

- An ordinary type from a transformer
    - A transformer can be turned into a non-transformer by using `Identity`

```haskell
type MyIdentity a = IdentityT Identity a
type Maybe a = MaybeT Identity a
type Either e a = EitherT e Identity a
type Reader r a = ReaderT e Identity a
type State s a = StateT s Identity a
```

```haskell
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
```

- See `OuterInner.hs`.
    - Base monad: Structurally outermost monad

********** BEGIN EXERCISE: WRAP IT UP **********

See `OuterInner.hs`.

********** END EXERCISE: WRAP IT UP **********

- `MonadTrans`

```haskell
fmap :: Functor f => (a -> b) -> f a -> f b

liftA :: Applicative f => (a -> b) -> f a -> f b

liftM :: Monad m => (a -> r) -> m a -> m r

-- `MonadTrans` is a typeclass that lifts.
class MonadTrans  twhere
    -- Lift a computation from the argument monad to the constructed monad
    lift :: Monad m => m a -> t m a
```

```haskell
newtype ScottyT e m a =
    ScottyT { runS :: State (ScottyState e m) a }
    deriving (Functor, Applicative, Monad)

newtype ActionT e m a =
    ActionT { runAM :: ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m )) a }
    deriving (Functor, Applicative)

type ScottyM = ScottyM Text IO
type ActionM = ActionT Text IO
```

- See `scotty.hs`.

- MonadTrans instances

```haskell
-- IdentityT
instance MonadTrans IdentityT where
    lift = IdentityT

-- MaybeT
instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just

lift :: Monad m => m a -> t m a

(MaybeT . liftM Just) :: Monad m => m a -> MaybeT m a

MaybeT :: m (Maybe a) -> MaybeT m a

(liftM Just) :: Monad m => m a -> m (Maybe a)

-- ReaderT
instance MonadTrans (ReaderT r) where
    lift = ReaderT . const
```

********** BEGIN EXERCISES: LIFT MORE **********

(FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)

```haskell
-- 1)
instance MonadTrans (EitherT e) where
    lift = EitherT . fmap Right

-- 2)
instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> m >>= \a -> return (a, s)
```

********** END EXERCISES: LIFT MORE **********

- Prolific lifting is the failure mode

```haskell
-- Some not-great code from Yesod

addSubWidget :: (YesodSubRoute sub master) => sub -> WidgetT sub master -> WidgetT sub' master a
addSubWidget sub w = do
    master <- liftHandler getYesod
    let sr = fromSubRoute sub master
    i <- WidgetT $ lift $ lift $ lift $ lift $ lift $ lift $ lift get
    w' <- liftHandler $ toMasterHandlerMaybe sr (const sub) Nothing $ flip runStateT i $ runWriterT $ runWriterT $ runWriterT $ runWriterT $ runWriterT $ runWriterT $ unWidgetT w

-- elided, pg no. 1542
```

- Wrap it, smack it, pre-lift it

- `MonadIO` aka zoom-zoom
    - More than one way to lift an action over additional structure
    - `MonadIO` keeps lifting actions over structure until it is lifted over all
      structure

```haskell
-- liftIO should satisfy the following constraints:
--
-- liftIO . return = return
-- liftIO (m >>= f) = liftIO m >>= (liftIO . f)

liftIO :: IO a -> ExceptT e IO a
liftIO :: IO a -> ReaderT r IO a
liftIO :: IO a -> StateT s IO a

liftIO :: IO a -> StateT s (ReaderT r IO) a
liftIO :: IO a -> ExceptT e (StateT s (ReaderT r IO)) a


class Monad m => MonadIO m where
    liftIO :: IO a -> m a
```

```haskell
instance MonadIO m => MonadIO (IdentityT m) where
    liftIO = IdentityT . liftIO

instance MonadIO m => MonadIO (EitherT e m) where
    liftIO = lift . liftIO
```

********** BEGIN EXERCISES: SOME EXERCISES **********

```haskell
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)

-- MaybeT
instance MonadIO m => MonadIO (MaybeT m) where
    liftIO = lift . liftIO

-- ReaderT
instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

-- StateT
instance MonadIO m => MonadIO (StateT s m) where
    liftIO = lift . liftIO
```

********** END EXERCISES: SOME EXERCISES **********

- Monad transformers in use

```haskell
-- MaybeT in use

-- github.com/wavewave/hoodle-core
recentFolderHook :: MainCoroutine (Maybe FilePath)
recentFolderHook = do
    xstate <- get

    (r :: Maybe FilePath) <- runMaybeT $ do
        hset <- hoist (view hookSet xstate)

        rfolder <- hoist (H.recentFolderHook hset)
        liftIO rfolder
    return r

-- github.com/devalot/hs-exceptions
addT :: FilePath -> FilePath -> IO (Maybe Integer)
addT f1 f2 = runMaybeT $ do
    s1 <- sizeT f1
    s2 <- sizeT f2
    return (s1 + s2)

-- github.com/wavewave/ghcjs-dom-delegator
main :: IO ()
main = do
    clickbarref <- asyncCallback1 AlwaysRetain clickbar
    clickbarref <- asyncCallback1 AlwaysRetain clickbaz

    r <- runMaybeT $ do
        doc <- MaybeT currentDocument
        bar <- lift . toJSRef =<< MaybeT (documentQuerySelector doc (".bar" :: JSString))
        baz <- lift . toJSRef =<< MaybeT (documentQuerySelector doc (".baz" :: JSString))

        lift $ do
            ref <- newObj
            del <- delegator ref
            addEvent bar "click" clickbarref
            addEvent baz "click" clickbazref

    case r of
        Nothing -> print "something wrong"
        Just _ -> print "well done"
```

```haskell
```
