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
    - `:info Sum` and `:info Product` say we can use those newtypes as monoids
      as long as they contain numeric values.

```haskell
-- (<>) is the infix operator for function `mappend`.
Prelude Data.Monoid> :t (<>)
(<>) :: Semigroup a => a -> a -> a
-- Doesn't work because "Frank" and "Herbert" are not numeric types.
Prelude Data.Monoid> Sum "Frank" <> Sum "Herbert"

<interactive>:31:1: error:
    • No instance for (Num [Char]) arising from a use of ‘<>’
    • In the expression: Sum "Frank" <> Sum "Herbert"
      In an equation for ‘it’: it = Sum "Frank" <> Sum "Herbert"
-- This is equivalent to `mappend (Sum 8) (Sum 9)`
Prelude Data.Monoid> (Sum 8) <> (Sum 9)
Sum {getSum = 17}
Prelude Data.Monoid> mappend mempty Sum 9
Sum {getSum = 9}
-- Doesn't work because `mappend` joins two things
Prelude Data.Monoid> mappend (Sum 8) (Sum 9) (Sum 10)

<interactive>:34:1: error:
    • Couldn't match expected type ‘Sum Integer -> t’
                  with actual type ‘Sum Integer’
    • The function ‘mappend’ is applied to three arguments,
      but its type ‘Sum Integer -> Sum Integer -> Sum Integer’
      has only two
      In the expression: mappend (Sum 8) (Sum 9) (Sum 10)
      In an equation for ‘it’: it = mappend (Sum 8) (Sum 9) (Sum 10)
    • Relevant bindings include it :: t (bound at <interactive>:34:1)
-- You can fix the issue of monoidial operations accepting only two arguments by
-- nesting them
Prelude Data.Monoid> mappend (Sum 1) (mappend (Sum 2) (Sum 3))
Sum {getSum = 6}
-- This is an equivalent operation using the infix operator
Prelude Data.Monoid> Sum 1 <> Sum 2 <> Sum 3
Sum {getSum = 6}
-- Or monoidial concatenation using a list type
Prelude Data.Monoid> mconcat [Sum 1, Sum 2, Sum 3]
Sum {getSum = 6}
Prelude Data.Monoid>
```

- Why bother?
    - Knowing monoids can help you recognize when you have a pattern.
    - Having principled laws around monoids helps combine monoidal operations
      safely.
    - Monoidal: You can define at least one law-abiding Monoid instance for it.

    - Monoids can be used to structure and describe common forms of data
      processing.
    - Say incrementally processing a large dataset.
    - Guarantees around parallel / concurrent / distributed aggregations (e.g.
      summation)
        - Identity values are sane defaults! Sane defaults are great for when an
          input may not be as expected / desired.
    - Abelian / commutative monoids are helpful when ordering of computations
      doesn't change the final result.

- Monoids and folding

```haskell
Prelude Data.Monoid> foldr mappend mempty ([2, 4, 6] :: [Product Int])
Product {getProduct = 48}
Prelude Data.Monoid> foldr mappend mempty ([2, 4, 6] :: [Sum Int])
Sum {getSum = 12}
Prelude Data.Monoid> foldr mappend mempty ["blah", "woot"]
"blahwoot"
Prelude Data.Monoid>
```

- Laws
    - Circumscribe and define our underlying algebra (set of operations)
    - Programs are proofs, and we want our programs to be provably correct


```haskell
-- left identity
mappend mempty x = x
-- right identity
mappend x mempty = x
-- associativity
mappend x (mappend y z) = mappend (mappend x y) z
-- mconcat
mconcat = foldr mappend mempty
```

In practice, this looks like:

```haskell
Prelude> import Data.Monoid
-- left identity
Prelude Data.Monoid> mappend mempty (Sum 1)
Sum {getSum = 1}
-- right identity
Prelude Data.Monoid> mappend (Sum 1) mempty
Sum {getSum = 1}
Prelude Data.Monoid>
-- `mappend` and `(<>)` should be identical in behavior.
--
-- associativity
Prelude Data.Monoid> (Sum 1) <> (Sum 2 <> Sum 3)
Sum {getSum = 6}
Prelude Data.Monoid> (Sum 1 <> Sum 2) <> (Sum 3)
Sum {getSum = 6}
-- `mconcat` is the same as `foldr mappend mempty`
Prelude Data.Monoid> mconcat [Sum 1, Sum 2, Sum 3]
Sum {getSum = 6}
Prelude Data.Monoid> foldr mappend mempty [Sum 1, Sum 2, Sum 3]
Sum {getSum = 6}
Prelude Data.Monoid>
```

```haskell
-- Use Monoid of lists
--
-- mempty is []
-- mappend is (++)
--
-- left identity
Prelude Data.Monoid> mappend mempty [1, 2, 3]
[1,2,3]
-- right identity
Prelude Data.Monoid> mappend [1, 2, 3] mempty
[1,2,3]
-- associativity
Prelude Data.Monoid> [1] <> ([2] <> [3])
[1,2,3]
Prelude Data.Monoid> ([1] <> [2]) <> [3]
[1,2,3]
-- `mconcat` = `foldr mappend mempty`
Prelude Data.Monoid> mconcat [[1], [2], [3]]
[1,2,3]
Prelude Data.Monoid> foldr mappend mempty [[1], [2], [3]]
[1,2,3]
Prelude Data.Monoid>
```

- Different instance, same representation
  - Many datatypes can have more than one valid monoid
  - Typical to use `newtype` to differentiate monoid usage
  - For numeric types, addition / multiplication map clearly to append, but for
    other datatypes it may be less clear.
    - More generally, `mappend` is not combining values, but condensing a set of
      values to a summary value

  - `Bool` types have two possible monoids - conjunction and disjunction

```haskell
Prelude> import Data.Monoid
-- `All` represents conjunction
--
-- `True` iff all values "appending" are `True`.
Prelude Data.Monoid> All True <> All True
All {getAll = True}
Prelude Data.Monoid> All True <> All False
All {getAll = False}
-- `Any` represents disjunction
--
-- `True` iff any value is `True`.
Prelude Data.Monoid> Any True <> Any False
Any {getAny = True}
Prelude Data.Monoid> Any False <> Any False
Any {getAny = False}
Prelude Data.Monoid> Any False <> Any True
Any {getAny = True}
Prelude Data.Monoid>
```

  - `Maybe` type has more than two possible monoids.
    - `First` and `Last` have the most obvious relationship (like boolean
      disjunction), with preference for one end of a series of `Maybe` values

```haskell
-- `First` returns leftmost non-`Nothing` value
Prelude Data.Monoid> First (Just 1) `mappend` First (Just 2)
First {getFirst = Just 1}
-- `Last` returns rightmost non-`Nothing` value
Prelude Data.Monoid> Last (Just 1) `mappend` Last (Just 2)
Last {getLast = Just 2}
Prelude Data.Monoid> Last Nothing `mappend` Last (Just 2)
Last {getLast = Just 2}
Prelude Data.Monoid> First Nothing `mappend` First (Just 2)
First {getFirst = Just 2}
-- Neither can return anything except `Nothing` if all values are `Nothing`
Prelude Data.Monoid> First Nothing `mappend` First Nothing
First {getFirst = Nothing}
Prelude Data.Monoid> Last Nothing `mappend` Last Nothing
Last {getLast = Nothing}
-- Using `First` with `Last` raises a typechecker exception.
Prelude Data.Monoid>
```

- Reusing algebras by asking for algebras
  - We can write the third type of monoid for `Maybe` vs. `First` and `Last`.

See `ReusingAlgebras.hs`.

********** BEGIN EXERCISE: OPTIONAL MONOID **********

See `OptionalMonoid.hs`.

********** END EXERCISE: OPTIONAL MONOID **********

- Associativity
  - Group arguments differently, get the same result

```haskell
-- Addition is associative
Prelude> (1 + 9001) + 9001
18003
Prelude> 1 + (9001 + 9001)
18003
-- Multiplication is associative
Prelude> (7 * 8) * 3
168
Prelude> 7 * (8 * 3)
168
-- Subtraction is not associative
Prelude> (1 - 10) - 100
-109
Prelude> 1 - (10 - 100)
91
Prelude>
```

- Associativity does not imply commutativity

```haskell
-- `(+)` and `(++)` may be both monoidal conjunctions. Let's test commutativity
-- for both of these operations.
--
-- `(+)` may be commutative
Prelude> evilPlus = flip (+)
Prelude> 76 + 67
143
Prelude> 76 `evilPlus` 67
143
-- `(++)` is definitely not commutative
Prelude> evilPlusPlus = flip (++)
Prelude> [1..3] ++ [4..6]
[1,2,3,4,5,6]
Prelude> [1..3] `evilPlusPlus` [4..6]
[4,5,6,1,2,3]
Prelude>
```

- `Monoid` abides by the law of associativity **but not the law of
  commutativity**

- Identity
