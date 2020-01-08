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

```haskell
-- `Functor` instance for two-tuple ignores first value inside the tuple, due to
-- kind-ness issues
Prelude> fmap (+1) ("blah", 0)
("blah",1)
Prelude> ("Woo", (+1)) <*> (" Hoo!", 0)
("Woo Hoo!",1)
Prelude>
```

```haskell
Prelude> import Data.Monoid
Prelude Data.Monoid> (Sum 2, (+1)) <*> (Sum 0, 0)
(Sum {getSum = 2},1)
Prelude Data.Monoid> (Product 3, (+9)) <*> (Product 2, 8)
(Product {getProduct = 6},17)
Prelude Data.Monoid> (All True, (+1)) <*> (All False, 0)
(All {getAll = False},1)
Prelude Data.Monoid>
```

- Tuple Monoid and Applicative side by side

```haskell
instance (Monoid a, Monoid b) => Monoid (a, b) where
    mempty = (mempty, mempty)
    (a, b) `mappend` (a', b') = (a `mappend` a', b `mappend` b')

instance Monoid a => Applicative ((,) a) where
    pure x = (mempty, x)
    (u, f) <*> (v, x) = (u `mappend` v, f x)
```

- Maybe Monoid and Applicative
    - (?) `Monoid` and `Applicative` aren't required / guaranteed "monoid of
      structure", functorial part may change way it behaves.

```haskell
instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    mappend m Nothing = m
    mappend Nothing m = m
    mappend (Just a) (Just a') = Just (mappend a a')

instance Applicative Maybe where
    pure = Just

    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    Just f <*> Just a = Just (f a)
```

- Applicative in use

- List Applicative

```haskell
-- 'f' here is roughly equivalent to the `[]` Functor
(<*>) ::  f  (a -> b) ->  f  a ->  f  b
(<*>) :: [ ] (a -> b) -> [ ] a -> [ ] b
(<*>) ::    [(a -> b)] -> [a] -> [b]

pure :: a -> f a
pure :: a -> [ ] a
```

```haskell
-- with `[]` Functor, we were mapping one function over many values
(2^) <$> [1, 2, 3]
[2, 4, 8]
-- with `[]` Applicative, we map many functions over many values
[(+1), (*2)] <*> [2, 4]
[3, 5, 4, 8]
```

```haskell
(<*>) :: Applicative f => f (a -> b) -> f a -> f b
-- f ~ []
listApply :: [(a -> b)] -> [a] -> [b]
listFmap :: (a -> b) -> [a] -> [b]
```

- That `listApply` doesn't return two lists or a nested list is from the
  monoidal part (binary associative operation with an identity, that returns one
  value of the same type)
- Function application over structure (functorial result) means no unapplied
  functions in the final result

```haskell
(,) <$> [1, 2] <*> [3, 4]
-- (NOTE: GHCi doesn't allow the partial result of fmapping partial tuple
-- construction with a list of values since it doesn't implement the Show
-- typeclass. Hence, manually list intermediate step as comment below.)
--
-- -> [(1, ), (2, )] <*> [3, 4]
[(1,3),(1,4),(2,3),(2,4)]
```

```haskell
Prelude> (,) <$> [1, 2] <*> [3, 4]
[(1,3),(1,4),(2,3),(2,4)]
Prelude> import Control.Applicative
Prelude Control.Applicative> liftA2 (,) [1, 2] [3, 4]
[(1,3),(1,4),(2,3),(2,4)]
Prelude Control.Applicative> (+) <$> [1, 2] <*> [3, 5]
[4,6,5,7]
Prelude Control.Applicative> liftA2 (+) [1, 2] [3, 5]
[4,6,5,7]
Prelude Control.Applicative> max <$> [1, 2] <*> [1, 4]
[1,4,2,4]
Prelude Control.Applicative> liftA2 max [1, 2] [1, 4]
[1,4,2,4]
Prelude Control.Applicative>
```

```haskell
-- `lookup`: A method that searches inside a list of tuples for a value that
--  matches the input, and returns paired value wrapped in `Maybe` context.
Prelude> :t lookup
lookup :: Eq a => a -> [(a, b)] -> Maybe b
Prelude> l = lookup 3 [(3, "hello")]
Prelude> l
Just "hello"
Prelude> :t l
l :: Maybe [Char]
Prelude> fmap length $ l
Just 5
Prelude> import Data.Char
Prelude Data.Char> c (x : xs) = toUpper x : xs
Prelude Data.Char> fmap c $ l
Just "Hello"
Prelude Data.Char>
```

See `ApplicativeFunctions.hs`.

```haskell
Prelude> :l ApplicativeFunctions.hs
[1 of 1] Compiling ApplicativeFunctions ( ApplicativeFunctions.hs, interpreted )
Ok, one module loaded.
*ApplicativeFunctions> f 3
Just "hello"
*ApplicativeFunctions> g 8
Just "chris"
-- I read the following line as "(++) fmap to 'f 3' from ApplicativeFunctions.hs apply to 'g 7' from ApplicativeFunctions.hs"
*ApplicativeFunctions> (++) <$> f 3 <*> g 7
Just "hellosup?"
*ApplicativeFunctions> (+) <$> h 5 <*> m 1
Just 9007
*ApplicativeFunctions> (+) <$> h 5 <*> m 6
Nothing
*ApplicativeFunctions> import Control.Applicative
*ApplicativeFunctions Control.Applicative> liftA2 (++) (g 9) (f 4)
Just "alohajulie"
*ApplicativeFunctions Control.Applicative> liftA2 (^) (h 5) (m 4)
Just 60466176
*ApplicativeFunctions Control.Applicative> liftA2 (*) (h 5) (m 4)
Just 60
*ApplicativeFunctions Control.Applicative> liftA2 (*) (h 1) (m 1)
Nothing
*ApplicativeFunctions Control.Applicative>
```

********** BEGIN EXERCISES: LOOKUPS **********

See `Lookups.hs`.

********** BEGIN EXERCISES: LOOKUPS **********

- Identity Applicative
    - Introduce structure without changing semantics of what you're doing

```haskell
-- f ~ Identity
-- Applicative f =>
type Id = Identity

(<*>) :: f (a -> b) -> f a -> f b
(<*>) :: Id (a -> b) -> Id a -> Id b

pure :: a -> f a
pure :: a -> Id a
```

```haskell
-- Map over Identity instead of elements within the list
Prelude> xs = [1, 2, 3]
Prelude> xs' = [9, 9, 9]
Prelude> const <$> xs <*> xs'
[1,1,1,2,2,2,3,3,3]
-- (PERSONAL NOTE: I'm not able to import `Identity` from modules
-- 'Data.Functor' or 'Control.Applicative'.)
```

- `Identity` enables mapping over `Identity` instead of over the list.

********** BEGIN EXERCISES: IDENTITY INSTANCE **********

See `IdentityInstance.hs`.

********** END EXERCISES: IDENTITY INSTANCE **********
