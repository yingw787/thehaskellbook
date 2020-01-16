# Chapter 20

- `Foldable`
    - Lists are foldable data structures
    - Foldability and catamorphisms are generalizable to many datatypes

- List fold: Reduce values inside list to one summary value, by recursively
  applying some function.
    - filter, map are folding operations that return a new list as summary value

- Folding is always dependent on instance with `Monoid` typeclass instance

- The `Foldable` class
    - Can be folded to a summary value
    - At minimum implements `foldMap` or `foldr`
        - `foldMap` and `foldr` can be implemented as part of each other
        - all other operations can be implemented as terms of either one

- In GHCi, may need to import `Data.Foldable` and `Data.Monoid`

- Revenge of the monoids
    - Folding implies binary associative operation that has an identity value
      (folds are monoidal)

```haskell
-- foldr (+) 0 [1..5]
-- 0     1   2 3
-- 0: Operation implemented by `Foldable` typeclass
-- 1: Associative operation executed by `Foldable` instance
-- 2: Identity value
-- 3: List from which to fetch two values at a time

-- `foldr` can work with a single argument given a monoidal type
Prelude> import Data.Foldable
Prelude Data.Foldable> import Data.Monoid
Prelude Data.Foldable Data.Monoid> xs = map Sum [1..5]
Prelude Data.Foldable Data.Monoid> fold xs
Sum {getSum = 15}
Prelude Data.Foldable Data.Monoid> xs = map Product [1..5]
Prelude Data.Foldable Data.Monoid> fold xs
Product {getProduct = 120}

-- For certain types, compiler and identiy and use the proper monoid without
-- needing to be explicit
Prelude Data.Foldable Data.Monoid> foldr (++) "" ["hello", " julie"]
"hello julie"
Prelude Data.Foldable Data.Monoid> fold ["hello", " julie"]
"hello julie"
```

- And now for something different

```haskell
-- `foldMap` first argument maps element of structure to `Monoid`.
Prelude Data.Foldable Data.Monoid> :t foldMap
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
Prelude Data.Foldable Data.Monoid> foldMap Sum [1..4]
Sum {getSum = 10}
Prelude Data.Foldable Data.Monoid> foldMap Product [1..4]
Product {getProduct = 24}
Prelude Data.Foldable Data.Monoid> foldMap All [True, False, True]
All {getAll = False}
Prelude Data.Foldable Data.Monoid> foldMap Any [(3 == 4), (9 > 5)]
Any {getAny = True}
Prelude Data.Foldable Data.Monoid> xs = [Just 1, Nothing, Just 5]
Prelude Data.Foldable Data.Monoid> foldMap First xs
First {getFirst = Just 1}
Prelude Data.Foldable Data.Monoid> foldMap Last xs
Last {getLast = Just 5}

-- `foldMap` function first argument can map a function to a value before using
-- Monoid `mappend` to reduce.
Prelude Data.Foldable Data.Monoid> foldMap Product [1..3]
-- 1 * 2 * 3 = 6
Product {getProduct = 6}
Prelude Data.Foldable Data.Monoid> xs = map Product [1..3]
Prelude Data.Foldable Data.Monoid> foldMap (*5) xs
-- 1 * 5 * 2 * 5 * 3 * 5 = 750
Product {getProduct = 750}
-- By contrast, `foldr` does not do map the value to each element in structure
Prelude Data.Foldable Data.Monoid> foldr (*) 5 [1..3]
-- 1 * 2 * 3 * 5
30

-- If method and identity are available, `foldr` doesn't even use Monoid
-- `mappend` and `mempty`.
Prelude Data.Foldable Data.Monoid> sumXs = map Sum [2..4]
Prelude Data.Foldable Data.Monoid> foldr (*) 3 sumXs
Sum {getSum = 72}
Prelude Data.Foldable Data.Monoid> productXs = map Product [2..4]
Prelude Data.Foldable Data.Monoid> foldr (*) 3 productXs
Product {getProduct = 72}

-- If `foldMap` gets passed one value, it doesn't use Monoid `mappend`.
Prelude Data.Foldable Data.Monoid> fm = foldMap (*5)
Prelude Data.Foldable Data.Monoid> fm (Just 100) :: Product Integer
Product {getProduct = 500}
Prelude Data.Foldable Data.Monoid> fm (Just 5) :: Sum Integer
Sum {getSum = 25}

-- `foldMap` will cast an empty value to `mempty`
Prelude Data.Foldable Data.Monoid> fm Nothing :: Sum Integer
Sum {getSum = 0}
Prelude Data.Foldable Data.Monoid> fm Nothing :: Product Integer
Product {getProduct = 1}
```

- Demonstrating Foldable instances

```haskell
-- Implementing `Foldable` for Identity
data Identity a = Identity a

instance Foldable Identity where
    foldr f z (Identity x) = f x z
    foldl f z (Identity x) = f z x
    foldMap f (Identity x) = f x

-- Prelude Data.Foldable Data.Monoid> :{
-- Prelude Data.Foldable Data.Monoid| -- Implementing `Foldable` for Identity
-- Prelude Data.Foldable Data.Monoid| data Identity a = Identity a
-- Prelude Data.Foldable Data.Monoid|
-- Prelude Data.Foldable Data.Monoid| instance Foldable Identity where
-- Prelude Data.Foldable Data.Monoid|     foldr f z (Identity x) = f x z
-- Prelude Data.Foldable Data.Monoid|     foldl f z (Identity x) = f z x
-- Prelude Data.Foldable Data.Monoid|     foldMap f (Identity x) = f x
-- Prelude Data.Foldable Data.Monoid| :}
-- Prelude Data.Foldable Data.Monoid> foldr (*) 1 (Identity 5)
-- 5
-- Prelude Data.Foldable Data.Monoid> foldr (*) 5 (Identity 5)
-- 25
-- Prelude Data.Foldable Data.Monoid> fm = foldMap (*5)
-- Prelude Data.Foldable Data.Monoid> type PI = Product Integer
-- Prelude Data.Foldable Data.Monoid> fm (Identity 100) :: PI
-- Product {getProduct = 500}
```

```haskell
-- Implementing `Foldable` for Maybe
data Optional x = Nada | Yep x

instance Foldable Optional where
    foldr _ z Nada = z
    foldr f z (Yep x) = f x z

    foldl _ z Nada = z
    foldl f z (Yep x) = f z x

    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a
```

```haskell
-- When the above is inputted into GHCi, you can get this output:
Prelude Data.Foldable Data.Monoid> foldMap (+1) Nada

-- Error because you're not implementing showable methods, doesn't matter if
-- you're deriving (Eq, Show).
<interactive>:130:1: error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘print’
      prevents the constraint ‘(Show a0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      These potential instances exist:
        instance (Show a, Show b) => Show (Either a b)
          -- Defined in ‘Data.Either’
        instance Show a => Show (First a) -- Defined in ‘Data.Monoid’
        instance Show a => Show (Last a) -- Defined in ‘Data.Monoid’
        ...plus 32 others
        ...plus 40 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In a stmt of an interactive GHCi command: print it
-- Solution is to cast it to a concrete type.
Prelude Data.Foldable Data.Monoid> foldMap (+1) Nada :: Sum Int
Sum {getSum = 0}
Prelude Data.Foldable Data.Monoid> foldMap (+1) Nada :: Product Int
Product {getProduct = 1}
Prelude Data.Foldable Data.Monoid> foldMap (+1) (Yep 1) :: Sum Int
Sum {getSum = 2}
```

- Some basic derived operations
