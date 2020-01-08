# Chapter 17

- Applicative
    - monoidal functors
        - function application lifted over structure (like `Funtor`)
        - function is also has some structure (like `Monoid` with binary
          associativity with identity)
        - Function and value both have structure

- Defining `Applicative`

```haskell
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

- `pure` lifts something into functorial (applicative) structure
- `<*>` (called "apply") which is like `fmap` with generic functorial structure

```haskell
Prelude> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
Prelude> :t (<$>)
(<$>) :: Functor f => (a -> b) -> f a -> f b
Prelude> :t (<*>)
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
Prelude> import Control.Applicative
-- `liftA` *is* `fmap`, except with constraint `Applicative` instead of
-- `Functor`.
Prelude Control.Applicative> :t liftA
liftA :: Applicative f => (a -> b) -> f a -> f b
-- `liftA2` and `liftA3` are like `fmap` with more arguments
Prelude Control.Applicative> :t liftA2
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
Prelude Control.Applicative> :t liftA3
liftA3
  :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
Prelude Control.Applicative>
```

- `Functor` vs. `Applicative`
