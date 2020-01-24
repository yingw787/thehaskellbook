# Chapter 25

- Composing types
    - Functors and applicatives, when composed, return another functor or
      applicative
    - Monads, when composed, may not return another monad
    - Otherwise, composing monads may allow us to have different effects

    - Monad transformer: type that takes in a monad as a type argument
        - `MaybeT`: transformer variant of `Maybe`

- Common functions as types
    - Use datatypes as helpers to demonstrate problems with monad composition

- Identity is boring

```haskell
newtype Identity a = Identity { runIdentity :: a }
```

- There is never a reason that Identity cannot be a newtype (since they are not
  sum or product types)

- Compose
    - Should look similar to function composition, but f and g represent type
      constructors, and not term-level functions

```haskell
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)
```

- Two functors sittin' in a tree, LIFTING

```haskell
-- Functor instance of Identity
instance Functor Identity where fmap f (Identity a) = Identity (f a)

-- Functor instance of Compose (possible if f and g both have Functor instances)
instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) =
        Compose $ (fmap . fmap) f fga
```

```haskell
newtype One f a = One (f a) deriving (Eq, Show)

instance Functor f => Functor (One f) where
    fmap f (One fa) = One $ fmap f fa


newtype Three f g h a = Three (f (g (h a))) deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
    fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha


v :: Compose [] Maybe (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]
```

- Twinplicative

```haskell
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    -- (PERSONAL NOTE: I feel pretty bad about skipping this, but I need to get
    -- this done, so, yeah.)
    --
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    pure = Compose $ pure (pure a)

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    (<*>) (Compose f) (Compose a) = liftA2 (<*>) f a
```

- Twonad?

```haskell
{-# LANGUAGE InstanceSigs #-}

-- impossible.
instance (Monad f, Monad g) => Monad (Compose f g) where
    return = pure

    (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
    (>>=) = ??? -- Not possible to do that last bind.
```

********** BEGIN EXERCISES: COMPOSE INSTANCES **********

```haskell
-- 1)
instance (Foldable f, Foldbale g) => Foldable (Compose f g) where
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    foldMap h (Compose fga) = foldMap (foldMap h) fga

-- 2)
instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    traverse h (Compose fga) = Compose <$> traverse (traverse h) fga
```

********** END EXERCISES: COMPOSE INSTANCES **********

- And now for something completely different

```haskell
class BiFunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

-- (FROM ANSWER KEY UNLESS OTHERWISE SPECIFIED)

-- 1)
data Deux a b = Deux a b

instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)

-- 2)
data Const a b = Const a

instance Bifunctor Const where
    bimap f g (Const a) = Const (f a)

-- 3)
data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)

-- 4)
data SuperDrei a b c = SuperDrei a b

instance Bifunctor SuperDrei where
    bimap f g (SuperDrei a b) = SuperDrei a (f b)

-- 5)
data SemiDrei a b c = SemiDrei a

instance Bifunctor SemiDrei where
    bimap f g (SemiDrei a) = SemiDrei a

-- 6)
data Quadriceps a b c d = Quadzzz a b c d

-- (CORRECT BY CHECKING ANSWER KEY)
instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadriceps a b c d) = Quadriceps a b (f c) (f d)

-- 7)
data Either a b = Left a | Right b

-- (CORRECT BY CHECKING ANSWER KEY)
instance Bifunctor (Either a) where
    bimap f g (Left a) = Left (f a)
    bimap f g (Right b) = Right (g b)
```

- Monad transformers
    - Need to reduce polymorphism by making one monad more concrete

- Monadic stacking
    - Sometimes you want a bind operation to address more than one monad at a
      time

- `IdentityT`

```haskell
newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)

newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)


instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Functor m => Functor (IdentityT m) where
    fmap f (IdentityT fa) = IdentityT (fmap f fa)


instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity (f a)

instance Applicative m => Applicative (IdentityT m) where
    pure x = IdentityT (pure x)
    (<*>) (IdentityT fab) (Identity fa) = IdentityT (fab <*> fa)


instance Monad Identity where
    return = pure
    (>>=) (Identity a) f = fa

instance Monad m => Monad (IdentityT m) where
    return = pure
    (>>=) (IdentityT ma) f = IdentityT $ ma >>= runIdentityT . f
```

- The bind breakdown

- Implementing the bind, step by step

```haskell
instance Monad m => Monad (IdentityT m) where
    return = pure

    (>>=) :: IdentityT m a -> (a -> IdentityT m b) -> IdentityT m b
    (>>=) (Identity ma) f =
        let aimb = (>>=) ma f
        in undefined
```
