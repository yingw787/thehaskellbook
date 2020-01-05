# Chapter 15

- Monoids and Semigroups

- What we mean by algebra
    - Study of mathematical symbols and usage governing their manipulation
    - Can be implemented with type classes
    - Set: type of operations are for
    - Instance: defines how each operation will perform for a given type or set

- Monoid
    - Binary associative operation with an identity
        - Monoid is the type class
        - Operation that is associative and has two arguments
        - A value within that type class that when combined with another value
          given an operation will always return that other value
    - Function that takes two arguments and follows two laws: associativity and
      identity
    - Typeclass that generalizes the above across type is the monoid

```haskell
Prelude> :i mappend
class Semigroup a => Monoid a where
...
mappend :: a -> a -> a
...
    -- Defined in ‘GHC.Base’
Prelude> mappend [1, 2, 3] [4, 5, 6]
[1,2,3,4,5,6]
Prelude> mappend [1..5] []
[1,2,3,4,5]
Prelude> mappend [] [1..5]
[1,2,3,4,5]
Prelude>
```

- How Monoid is defined in Haskell

```haskell
Prelude> :i Monoid
class Semigroup a => Monoid a where
  -- 'mempty` is the identity value
  mempty :: a
  -- 'mappend' is the binary associative operation
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  {-# MINIMAL mempty #-}
        -- Defined in ‘GHC.Base’
instance Monoid [a] -- Defined in ‘GHC.Base’
instance Monoid Ordering -- Defined in ‘GHC.Base’
instance Semigroup a => Monoid (Maybe a) -- Defined in ‘GHC.Base’
instance Monoid a => Monoid (IO a) -- Defined in ‘GHC.Base’
instance Monoid b => Monoid (a -> b) -- Defined in ‘GHC.Base’
instance (Monoid a, Monoid b, Monoid c, Monoid d, Monoid e) =>
         Monoid (a, b, c, d, e)
  -- Defined in ‘GHC.Base’
instance (Monoid a, Monoid b, Monoid c, Monoid d) =>
         Monoid (a, b, c, d)
  -- Defined in ‘GHC.Base’
instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)
  -- Defined in ‘GHC.Base’
instance (Monoid a, Monoid b) => Monoid (a, b)
  -- Defined in ‘GHC.Base’
instance Monoid () -- Defined in ‘GHC.Base’
```

- Examples of using Monoid

```haskell
-- Lists are an example of a type with typeclass Monoid
Prelude> mappend [1, 2, 3] [4, 5, 6]
[1,2,3,4,5,6]
Prelude> mconcat [[1..3], [4..6]]
[1,2,3,4,5,6]
Prelude> mappend "Trout" " goes well with garlic"
"Trout goes well with garlic"
-- This monoid maps well to existing operator `(++)`
Prelude> (++) [1, 2, 3] [4, 5, 6]
[1,2,3,4,5,6]
Prelude> (++) "Trout" " goes well with garlic"
"Trout goes well with garlic"
Prelude> foldr (++) [] [[1..3], [4..6]]
[1,2,3,4,5,6]
Prelude> foldr mappend mempty [[1..3], [4..6]]
[1,2,3,4,5,6]
Prelude>
```

- Why Integer doesn't have a Monoid
    - No numeric types have `Monoid` instances.
    - The monoid of numbers is summation, but it could also be multiplication.
    - Therefore, two numeric types could be added or multiplied as part of a
      `mappend` operation.
    - Therefore, use `Sum` and `Product` types in order to get the appropriate
      Monoid.

```haskell
Prelude> x = 1 :: Integer
Prelude> y = 3 :: Integer
Prelude> mappend x y

<interactive>:16:1: error:
    • No instance for (Monoid Integer) arising from a use of ‘mappend’
    • In the expression: mappend x y
      In an equation for ‘it’: it = mappend x y
-- Use the `Sum` and `Product` newtypes to signal which monoid you want.
-- Import module `Data.Monoid` to get access to these.
Prelude> import Data.Monoid
Prelude Data.Monoid> mappend (Sum 1) (Sum 5)
Sum {getSum = 6}
Prelude Data.Monoid> mappend (Product 5) (Product 5)
Product {getProduct = 25}
-- `Sum` and `Product` work for typeclass `Num`.
Prelude Data.Monoid> mappend (Sum 4.5) (Sum 3.4)
Sum {getSum = 7.9}
Prelude Data.Monoid>
```

-   Why use `newtype` to enforce different monodial behaviors?
    -   There's not a whole lot of difference between the following datatypes:

        ```haskell
        data Server = Server String
        newtype Server' = Server' String
        ```

    -   `newtype` constrains the datatype to having a single unary data
        constructor (cannot grow into a sum or product type)
    -   To improve type safety
    -   No additional runtime overhead in wrapping the original type
        -   (PERSONAL NOTE: It looks like `newtype` is critically important to
            building type classes with control flow, since you care about the
            control flow aspect than the actual data aspect)

- More on `Sum` and `Product`
