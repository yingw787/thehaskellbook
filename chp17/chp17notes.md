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
    - Any `Applicative` also has a `Functor`, and you can define a `Functor`
      w.r.t. a provided `Applicative`.

```haskell
fmap f x = pure f <*> x
```

```haskell
Prelude> fmap (+1) [1, 2, 3]
[2,3,4]
Prelude> pure (+1) <*> [1, 2, 3]
[2,3,4]
Prelude>
```

```haskell
-- `pure` lifts a value into functorial structure
--
-- Since "functorial structure" is quite abstract, should cast to different
-- Functors using `::`.
--
-- Functor `[]`
Prelude> pure 1 :: [Int]
[1]
-- Functor `Maybe`
Prelude> pure 1 :: Maybe Int
Just 1
-- Functor `Either`
--
-- Functors with multiple arguments have arguments lifted into the last type
-- argument, just as with `fmap` and `()` (tuple).
Prelude> pure 1 :: Either a Int
Right 1
-- Functor `()`
Prelude> pure 1 :: ([a], Int)
([],1)
Prelude>
```

- Applicative functors are monoidal functors

```haskell
-- ($) does nothing except change operation precedence. Said another way,
-- it could represent ordinary function application.
 ($)  ::   (a -> b) ->   a ->   b
 -- (<$>) lifts (a -> b) over 'f' wrapped around value and applies the function
 -- to that value.
(<$>) ::   (a -> b) -> f a -> f b
-- (<*>) has function embedded in functorial structure
--
-- Arguments to the function are `f (a -> b)` and `f a`.
(<*>) :: f (a -> b) -> f a -> f b
```

```haskell
:: f (a -> b) -> f a -> f b
-- If 'f' is a type constrained with 'Monoid', then it can take two values of
-- the same type and return one value of the same type.
--
-- Prelude> :t mappend
-- mappend :: Monoid a => a -> a -> a
-- Prelude>
   f             f      f
   (a -> b)      a      b
```

```haskell
mappend  :: f          -> f   -> f
($)      ::   (a -> b) ->   a ->   b

(<*>)    :: f (a -> b) -> f a -> f b
```

```haskell
-- `applicative` with `[]` functor
--
-- We can use a *list of functions* to lift over our list of values, instead of
-- just having one function to lift over our list of values as with plain
-- Functor.
--
-- (PERSONAL NOTE: This seems a lot like effective matrix multiplication, or
-- matrix operations in Haskell, and being to vary the operation done to each
-- column, which would be extremely helpful in dataframe transformations for a
-- possible Haskell data engineering framework like a "pandas" analogue...)
--
-- (PERSONAL NOTE: I wonder how well it works with the C FFI...)
Prelude> [(*2), (*3)] <*> [4, 5]
-- [2 * 4, 2 * 5, 3 * 4, 3 * 5]
[8,10,12,15]
Prelude>
```

```haskell
-- `Maybe` maps over possibility of value's non-existence.
-- With `Applicative`, function also may not be provided.
Prelude> Just (*2) <*> Just 2
Just 4
Prelude> Just (*2) <*> Nothing
Nothing
Prelude> Nothing <*> Just 2
Nothing
Prelude> Nothing <*> Nothing
Nothing
Prelude>
```

- Show me the monoids
