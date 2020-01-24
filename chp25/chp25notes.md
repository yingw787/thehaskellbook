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
