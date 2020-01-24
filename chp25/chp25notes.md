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
instance Functor Identity where fmap f (Identity a) = Identity (f a)
```
