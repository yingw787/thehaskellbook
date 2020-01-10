# Chapter 18

- Monad
  - `Applicative`s are monoidal functors
  - `Monad`s are applicative functors, but also have something else special about them

- Sorry - a monad is not a burrito
  - `Functor` maps a function over some structure
  - `Applicative` maps a function contianed in some structure, over some
    structure, and combines the two layers of structure
  - `Monad` is another way of applying functions over structure

```haskell
Prelude> :i Monad
class Applicative m => Monad (m :: * -> *) where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  fail :: String -> m a
  {-# MINIMAL (>>=) #-}
```

- `Applicative` m
  - Older versions of GHC did not have `Applicative` as a superclass of `Monad`.
  - You can derive `Applicative` and `Functor` in terms of `Monad`, as you can
    derive `Functor` in terms of `Applicative`.

```haskell
-- fmap f xs = xs >>= return . f
--
-- This is a law; `Functor`, `Applicative`, and `Monad` instances over a given
-- type should have the same core behavior
--
Prelude> fmap (+1) [1..3]
[2,3,4]
Prelude> [1..3] >>= return . (+1)
[2,3,4]
```

- `Functor` -> `Applicative` -> `Monad`
