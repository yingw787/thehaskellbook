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
