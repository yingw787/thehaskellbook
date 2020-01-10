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

- Core operations

```haskell
-- (>>=) is the binding operator; this is the operator that makes Monads
-- special.
Prelude> :t (>>=)
(>>=) :: Monad m => m a -> (a -> m b) -> m b
-- (>>) is the sequencing operator. (>>) sequences two actions, discarding any
-- value of the first action.
Prelude> :t (>>)
(>>) :: Monad m => m a -> m b -> m b
-- 'return' is the same as 'pure', lifting a value inside your structure
Prelude> :t return
return :: Monad m => a -> m a
```

- The novel part of `Monad`
  - `return` is to `Monad` what `pure` is to `Applicative`
  - `(>>)` has a counterpart in `Applicative` too

  - `(>>=)` is visibly similar to `fmap` and `<*>`.

```haskell
-- To make (>>=) maximally similar to `fmap` and `<*>`:
fmap  :: Functor      f =>   (a -> b) ->       f a  -> f b
<*>   :: Applicative  f => f (a -> b) ->       f a  -> f b
>>=   :: Monad        f => f  a       -> (a -> f b) -> f b

-- If 'b' in def 'fmap' is roughly equivalent to 'f b' in 'fmap':
fmap :: Functor f => (a -> f b) -> f a -> f (f b)
```

```haskell
Prelude> andOne x = [x, 1]
Prelude> andOne 10
[10,1]
Prelude> :t fmap andOne [4, 5, 6]
fmap andOne [4, 5, 6] :: Num a => [[a]]
-- 'fmap' here generates more structure; list nested inside of a list
Prelude> fmap andOne [4, 5, 6]
[[4,1],[5,1],[6,1]]
-- Flatten the internal structure, for list struct type only
Prelude> concat $ fmap andOne [4, 5, 6]
[4,1,5,1,6,1]
Prelude>
```

- `Monad` is a generalization of the 'concat' method (extending it beyond
  `Foldable`)

```haskell
Prelude> import Control.Monad (join)
Prelude Control.Monad> :t join
join :: Monad m => m (m a) -> m a
Prelude Control.Monad> :t concat
concat :: Foldable t => t [a] -> [a]
Prelude Control.Monad>
```

- Allowing the function to flatten structure is not something present in either
  `Functor` or `Applicative`, but is allowed in `Monad`
  - `fmap` can inject more structure, but can't merge structure
  - `join` and `map` give `(>>=)` or `bind`.

********** BEGIN EXERCISE: THE ANSWER IS THE EXERCISE **********

```haskell
bind :: Monad m => (a -> m b) -> m a -> m b
-- (FROM ANSWER KEY)
bind f m = join $ fmap f m

-- Prelude Control.Monad> :t (fmap . join)
-- (fmap . join) :: Functor f => (a -> a -> b) -> f a -> f b
-- Prelude Control.Monad> :t (fmap $ join)
-- (fmap $ join) :: (Monad m, Functor f) => f (m (m a)) -> f (m a)
-- Prelude Control.Monad> :t (fmap join)
-- (fmap join) :: (Monad m, Functor f) => f (m (m a)) -> f (m a)
-- Prelude Control.Monad> :t (join $ fmap)
-- (join $ fmap) :: (a -> a) -> a -> a
-- Prelude Control.Monad> :t (join fmap)
-- (join fmap) :: (a -> a) -> a -> a
```

********** BEGIN EXERCISE: THE ANSWER IS THE EXERCISE **********

- What `Monad` is not
  - Not impure: monadic functions are pure functions. `IO` allows for impure, or
    effectful, actions, and `IO` is not `Monad`.
  - Not embedded imperative programming: Monads can be used to sequence
    operations, but there are commutative monads like `Reader` that don't order
    actions.
  - Not a value. `Monad` is a typeclass in general vernacular.
  - Not always strict. Monadic operations `bind` and `return` are lazy, while
    some operations can be made strict within a specific instance.

- `Monad` also lifts!
  - `liftM` method set exists for `Monad` because `Applicative` wasn't
    discovered until much later
    - (PERSONAL NOTE: Using the word "discover" here is pretty interesting)
  - `liftM` is pretty much `liftA` with a different typeclass constraint

```haskell
Prelude> import Control.Monad (liftM)
Prelude Control.Monad> import Control.Applicative (liftA)
Prelude Control.Monad Control.Applicative> :t liftA
liftA :: Applicative f => (a -> b) -> f a -> f b
Prelude Control.Monad Control.Applicative> :t liftM
liftM :: Monad m => (a1 -> r) -> m a1 -> m r
```
