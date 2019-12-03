# Chapter 6

- Typeclasses

- Kind of the opposite of types
    - Type declarations define how types are created
    - Typeclasses define how set of types are consumed or used.

- You should be able to add new cases and new functions to a data type without
  recompilation and retain static type safety (e.g. no type casting)

- Type classes are like interfaces to data
    - `Eq` implements equality / comparison checks and provides a way for data
      types to use equality operators
    - `Num` implements numeric operators

```haskell
Prelude> :info Bool
data Bool = False | True 	-- Defined in ‘GHC.Types’
-- Can be tested for equality
instance Eq Bool -- Defined in ‘GHC.Classes’
-- Can be put into a sequential order (strictly ordered)
instance Ord Bool -- Defined in ‘GHC.Classes’
-- Renders things into strings
instance Show Bool -- Defined in ‘GHC.Show’
-- Parses strings into things. Don't use it (PERSONAL NOTE: probably
-- something like `eval`, which results is severe security issues)
instance Read Bool -- Defined in ‘GHC.Read’
-- Can be enumerated
instance Enum Bool -- Defined in ‘GHC.Enum’
-- Has an upper / lower bound
instance Bounded Bool -- Defined in ‘GHC.Enum’
```

- Typeclasses have a heirarchy
    - All `Fractional` implements `Num`, but not all `Num` implements
      `Fractional`.
    - All `Ord` implements `Eq`
    - All `Enum` implements `Ord`.
    - To be able to put something into an enumerated list, they must be ordered;
      to order something, they must be able to be compared.

- Some datatypes cannot implement some typeclasses (e.g. functions cannot
  implement equality `:info (->)`)

- Typeclasses allow for compile-time checking of operations to see whether it is
  valid for a given data type.

- Haskell does not provide universal stringification (`Show`) or equality
  (`Eq`).

```haskell
-- Define a new data type called `Trivial`.
Prelude> data Trivial = Trivial
-- Since (==) is not implemented, and because data type does not derive from
-- `Eq` typeclass, this results in an exception.
Prelude> Trivial == Trivial

<interactive>:11:1: error:
    • No instance for (Eq Trivial) arising from a use of ‘==’
    • In the expression: Trivial == Trivial
      In an equation for ‘it’: it = Trivial == Trivial
-- We can implement a method that checks this data type for equality.
Prelude> :{
Prelude| data Trivial = Trivial'
Prelude| instance Eq Trivial where
--       0        1  2       3
Prelude| Trivial' == Trivial' = True
--       4        5  6          7
Prelude| :}
-- 0. `instance` declares a typeclass instance.
-- 1. Typeclass the `instance` is providing.
-- 2. Data type instance is provided for. Here, it is implementing the
-- `Eq` typeclass for the `Trivial` data type.
-- 3. `where` terminates initial declaration and begins declaration.
-- 4. `Trivial'` is the data constructor as first argument to the
-- (==) operation.
-- 5. Operator in declaration.
-- 6. Second argument of operator.
-- 7. Result of this operation.

<interactive>:20:10: warning: [-Wmissing-methods]
    • No explicit implementation for
        either ‘Prelude.==’ or ‘/=’
    • In the instance declaration for ‘Eq Trivial’
-- One expression that can be constructed from definition.
Prelude> Trivial' == Trivial'
True
Prelude>
```

- Implement data type `dayOfWeek` and `Date`. See `dates.hs`.

```haskell
*Dates> Date Thu 10 == Date Thu 10
True
*Dates> Date Thu 10 == Date Thu 11
False
*Dates> Date Thu 10 == Date Weds 10
False
*Dates>
```

- Partial function: One that doesn't handle all the possible cases.
    - Different from partial application of functions.
    - (PERSONAL NOTE: So these do exist in Haskell)
    - Must avoid partial functions where possible. In GHCi, type `:set -Wall` to
      turn on all warnings. This may be annoying if you are writing a method to
      do type casting:

        ```haskell
        *Dates> :set -Wall
        *Dates> :{
        *Dates| f :: Int -> Bool
        *Dates| f 2 = True
        *Dates| :}

        <interactive>:64:1: warning: [-Wincomplete-patterns]
            Pattern match(es) are non-exhaustive
            In an equation for ‘f’:
                -- GHCi is mad because you are not handling cases where latter
                -- argument is not in set {2}. Assuming you have `:set -Wall`.
                Patterns not matched: p where p is not one of {2}
        *Dates>
        ```
    - Answer here is to have an catch-all case that matches everything, or use a
      data type that isn't huge like `Int`.
    - Use Enum or bounded data types in order to fully specify everything and
      have everything else be compile-time errors.

- You can specify typeclasses when implementing other typeclasses. See
  `identity.hs`.

```haskell
Prelude> :r
[1 of 1] Compiling Identity         ( identity.hs, interpreted )
-- Ok, one module loaded. (commenting out due to syntax highlighting
-- issues in markdown)
*Identity> data NoEq = NoEqInst deriving Show
*Identity> inoe = Identity NoEqInst
-- When trying to execute (==), because data constructor NoEq derives Show,
-- and because inoe is also an instance of Identity, (==) is undefined for the
-- specified types.
*Identity> inoe == inoe

<interactive>:71:1: error:
    • No instance for (Eq NoEq) arising from a use of ‘==’
    • In the expression: inoe == inoe
      In an equation for ‘it’: it = inoe == inoe
*Identity>
```

********** BEGIN EXERCISES: EQ INSTANCES **********

Write the `Eq` instance for the datatype provided.

```haskell
-- (1)
data TisAnInteger = TisAn Integer
-- (2)
data TwoIntegers = Two Integer Integer
-- (3)
data StringOrInt = TisAnInt Int | TisAString String
-- (4)
data Pair a = Pair a a
-- (5)
data Tuple a b = Tuple a b
-- (6)
data Which a = ThisOne a | ThatOne a
-- (7)
data EitherOr a = Hello a | Goodbye b
```

__________

See `eq_instances.hs`.

********** END EXERCISES: EQ INSTANCES **********

- Num: type class implemented by most numeric types.

```haskell
Prelude> :info Num
class Num a where
-- Predefined functions
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-} -- Defined in ‘GHC.Num’
-- List of instances
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’
Prelude>
```

```haskell
Prelude> :info Integral
-- Type class constraints for Integral on Real and Enum.
-- Type class inheritance (Real <- Num) is only additive, multiple
-- inheritance problem "diamond of death" is avoided.
class (Real a, Enum a) => Integral a where
  quot :: a -> a -> a
  rem :: a -> a -> a
  div :: a -> a -> a
  mod :: a -> a -> a
  quotRem :: a -> a -> (a, a)
  divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
  {-# MINIMAL quotRem, toInteger #-}
  	-- Defined in ‘GHC.Real’
instance Integral Word -- Defined in ‘GHC.Real’
instance Integral Integer -- Defined in ‘GHC.Real’
instance Integral Int -- Defined in ‘GHC.Real’
Prelude>
```

********** START EXERCISES: TUPLE EXPERIMENT **********

Look at the types given for `quotRem` and `divMod`. What do you think those
functions do? Test your hypotheses by playing with them in the REPL. We've given
you a sample to start with below:

`Prelude> ones x = snd (divMod x 10)`

```haskell
Prelude> :t quotRem
quotRem :: Integral a => a -> a -> (a, a)
Prelude> :t divMod
divMod :: Integral a => a -> a -> (a, a)
```

I think `divMod` divides two input arguments of type `Integral`, and generates a
tuple result of two result values of type `Integral`, the former being the
quotient and the latter being the modulus.

I think `quotRem` might do the same thing.

(CORRECT, as far as I know: https://stackoverflow.com/a/342379)

********** END EXERCISES: TUPLE EXPERIMENT **********

- Fractional
    - Type constraint of `Num` in type class.

```haskell
-- No type signature, works fine due to type inference.
Prelude> divideThenAdd x y = (x / y) + 1
Prelude> :{
-- Providing an illegal type signature results in an compile-time error.
Prelude| divideThenAdd :: Num a => a -> a -> a
Prelude| divideThenAdd x y = (x / y) + 1
Prelude| :}

<interactive>:60:22: error:
    • Could not deduce (Fractional a) arising from a use of ‘/’
      from the context: Num a
        bound by the type signature for:
                   divideThenAdd :: forall a. Num a => a -> a -> a
        at <interactive>:59:1-37
      Possible fix:
        add (Fractional a) to the context of
          the type signature for:
            divideThenAdd :: forall a. Num a => a -> a -> a
    • In the first argument of ‘(+)’, namely ‘(x / y)’
      In the expression: (x / y) + 1
      In an equation for ‘divideThenAdd’: divideThenAdd x y = (x / y) + 1
Prelude> :{
-- Provide the correct type signature in order to ensure correctness.
Prelude| divideThenAdd :: Fractional a => a -> a -> a
Prelude| divideThenAdd x y = (x / y) + 1
Prelude| :}
Prelude>
```

********** BEGIN EXERCISES: PUT ON YOUR THINKING CAP **********

Why didn't we need to make the type of the function we wrote require both type
classes?? Why didn't we have to do this:

```haskell
f :: (Num a, Fractional a) => a -> a -> a
```

Consider what it means for something to be a *subset* of a larger set of
objects.

__________

Since `Fractional` is a subset of `Num`, assuming that input argument is
`Fractional`, its data type already understands all operations handled by `Num`.
Furthermore, since type class inheritance is additive only, there is no
ambiguity between different type classes reimplementing the same method.
Therefore, there are no compile-time errors.

********** END EXERCISES: PUT ON YOUR THINKING CAP **********

- Type defaults
    - Polymorphic types must resolve to concrete types.
    - Most times, concrete type would come from type signature specified, or
      from type inference.
    - Sometimes though, there may not be a concrete type, esp. when working in
      GHCi REPL.
    - [The Haskell Report](https://www.haskell.org/onlinereport/haskell2010/)
      specifies default concrete types for numeric type classes.

- Ord

```haskell
Prelude> :info Ord
-- Type class constraint of Eq.
class Eq a => Ord a where
-- List of operations.
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}
    -- Defined in ‘GHC.Classes’
-- List of instances for type classes.
instance Ord a => Ord [a] -- Defined in ‘GHC.Classes’
instance Ord Word -- Defined in ‘GHC.Classes’
instance Ord Ordering -- Defined in ‘GHC.Classes’
instance Ord Int -- Defined in ‘GHC.Classes’
instance Ord Float -- Defined in ‘GHC.Classes’
instance Ord Double -- Defined in ‘GHC.Classes’
instance Ord Char -- Defined in ‘GHC.Classes’
instance Ord Bool -- Defined in ‘GHC.Classes’
-- Truncated instances.
Prelude>
```

```haskell
Prelude> compare 7 8
LT
Prelude> compare 4 (-4)
GT
Prelude> compare "Julie" "Chris"
GT
Prelude> compare True False
GT
Prelude> compare False True
LT
Prelude> compare True True
EQ
Prelude>
```

```haskell
-- `max` operation available to data types implementing `Ord` typeclass.
--
-- Error message is not intuitive, but `max` is not a method that can be
-- curried.
--
-- Typeclass that couldn't be found was `Show`, that allows values to be
-- printed to the terminal. We called `print` because we are using GHCi.
Prelude> max "Julie"

<interactive>:76:1: error:
    • No instance for (Show ([Char] -> [Char]))
        arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In a stmt of an interactive GHCi command: print it
Prelude>
```

- Ord implies Eq

```haskell
Prelude> :{
-- No type constraint results in a compile-time error.
Prelude| check' :: a -> a -> Bool
Prelude| check' a a' = (==) a a'
Prelude| :}

<interactive>:12:15: error:
    • No instance for (Eq a) arising from a use of ‘==’
      Possible fix:
        add (Eq a) to the context of
          the type signature for:
            check' :: forall a. a -> a -> Bool
    • In the expression: (==) a a'
      In an equation for ‘check'’: check' a a' = (==) a a'
Prelude> :{
-- However, specifying a subset of `Eq` is fine, since `Ord` inherits
-- operations needed by `Eq`.
Prelude| check' :: Ord a => a -> a -> Bool
Prelude| check' a a' = (==) a a'
Prelude| :}
Prelude>
```

********** START EXERCISES: WILL THEY WORK? **********

Take a look at the following code examples and try to decide if they will work,
what result they will return if they do, and why or why not (be sure, as always,
to test them in your REPL once you have decided on your answer):

1.  Shouldn't work. `length` operands are both working on input arguments of type
    `Foldable`, which should return a result of type `Integral`, which should be
    constrained by `Ord` since it is a numeric type. However, since no
    parentheses are around both `length` arguments, and the expression is
    left-associative, `max` will only be operating on one input argument, which
    will result in an exception.

    (INCORRECT, parentheses are executed first.)

    ```haskell
    Prelude> max (length [1, 2, 3]) (length [8, 9, 10, 11, 12])
    5
    Prelude>
    ```

2.  Should work. Multiplication operations should result in abstract class `Num`
    return values. `compare` will be operating on two `Num` values, which should
    resolve to `Bool` successfully.

    (PARTIALLY CORRECT, resolves to `Ordering` data type.)

    ```haskell
    Prelude> compare (3 * 4) (3 * 5)
    LT
    Prelude>
    ```

3.  Shouldn't work. `compare` only works on values of the same data type, or
    that can be resolved to the same data type.

    ```haskell
    Prelude> compare "Julie" True

    <interactive>:32:17: error:
        • Couldn't match expected type ‘[Char]’ with actual type ‘Bool’
        • In the second argument of ‘compare’, namely ‘True’
        In the expression: compare "Julie" True
        In an equation for ‘it’: it = compare "Julie" True
    Prelude>
    ```

4.  Should work. Parentheses are resolved first, which results in two `Num`
    return values, which can be executed with `>` as `Num` inherits from `Ord`.

    (CORRECT)

    ```haskell
    Prelude> (5 + 3) > (3 + 6)
    False
    Prelude>
    ```

********** END EXERCISES: WILL THEY WORK? **********
