# Chapter 5

- Types

- You cannot create untyped data in Haskell

- Typed lambda calculus (System F) in the 1970s
    - Haskell improves upon System F
        - Allows general recursion
        - Hindley-Milner type system

- Type systems in logic / mathematics designed to impose constraints that
  enforce correctness.
- Well-designed type systems help eliminate some classes of errors
- Also restructure concerns such as what a conditional over a non-Boolean value
  might be
- Type system defines associations between different parts of a program
- Checks all parts fit together in logically consistent, provably correct way.

- Haskell applies static typing, which means type-checking occurs at compile
  time.
- Doesn't prevent possibilities for runtime errors and exceptions.
- Type system reduces number and kinds of tests you must write.

- Good type systems also enable compiler optimizations, because the compiler can
  know and predict certain things about program execution.
- Types can also act as documentation and improve hallway usability / pair
  programming.

- Type systems will require a lot of upfront work, with a later payoff: safer,
  more maintainable code.

```haskell
Prelude> :type 't'
't' :: Char
Prelude> :type "julie"
"julie" :: [Char]
Prelude> :type True
True :: Bool
-- 13 may look like an integer, but that would only allow us to use it in
-- computations that take integers. Compiler gives it the type with the broadest
-- applicability (most polymorphic) and constrains it based on that
-- (constrained polymorphism).
Prelude> :type 13
13 :: Num p => p
-- Types can also match function type signatures.
Prelude> :type not
not :: Bool -> Bool
Prelude>
```

```haskell
Prelude> :info (->)
-- (->) is the type constructor for Haskell methods.
-- Has no data constructors and has no data constructors.
-- There are no data constructors because the value that shows up at term level
-- is the function. Functions are values.
--
-- a is the input to the function, b is the return value of the function.
data (->) (a :: TYPE q) (b :: TYPE r) 	-- Defined in ‘GHC.Prim’
-- Note similarity between (->) and (,).
Prelude> :info (,)
data (,) a b = (,) a b 	-- Defined in ‘GHC.Tuple’
Prelude>
```

- Functions are different from regular values because they can be applied.
- `(->)` is an infix operator that has two parameters and associates to the right
    - Function *application* is still left-associative.

```haskell
Prelude> :type fst
-- Method `fst` to get the first element of a two-tuple
--
-- First parameter of `fst` is '(a, b)' (which is a two-tuple with its own
-- type definition), an infix operator that defines the actual function,
-- and the return value 'a', which is the same 'a' that is in the tuple.
--
-- We know that it is the same 'a' that is returned because we see the input 'a'
-- and output 'a' are the same type, and nothing happens between input and
-- output. Note that the second deduction is made from the type signature (?).
fst :: (a, b) -> a
-- Another method `length` takes in an input of subclass `Foldable`, and return
-- an integer. Method `length` does not care about internal arguments of 'a'.
Prelude> :type length
length :: Foldable t => t a -> Int
Prelude>
```

- Compiler does not limit types to concrete ones, but rather limits based on
  polymorphic type variable, to enable the variable to represent any concrete
  type.
- For numbers, this means we can represent the same numeric value in different
  types.

```haskell
Prelude> fifteen = 15
Prelude> :type fifteen
fifteen :: Num p => p
Prelude> fifteenInt = fifteen :: Int
Prelude> :type fifteenInt
fifteenInt :: Int
Prelude> fifteenDouble = fifteen :: Double
Prelude> :type fifteenDouble
fifteenDouble :: Double
Prelude>
```

- `Int` and `Double` both have an instance of the `Num` type class, which is why
  we can cast `Num a => a` 'fifteen' to both 'Int' and 'Double'.

- Constrained polymorphism also means we can apply `(+)` where we could not if we
  used concrete types only, to avoid type translation errors:

```haskell
-- 'fifteen' is cast to Double.
Prelude> fifteenDouble + fifteen
30.0
-- 'fifteen' is cast to Int.
Prelude> fifteenInt + fifteen
30
Prelude> fifteenDouble + fifteenDouble
30.0
Prelude> fifteenInt + fifteenInt
30
-- Type translation error by adding two different concrete types.
-- Since we applied 'fifteenDouble' as the first argument, 'Double' is the
-- expected type for the second argument, but the actual type was 'Int'.
Prelude> fifteenDouble + fifteenInt

<interactive>:21:17: error:
    • Couldn't match expected type ‘Double’ with actual type ‘Int’
    • In the second argument of ‘(+)’, namely ‘fifteenInt’
      In the expression: fifteenDouble + fifteenInt
      In an equation for ‘it’: it = fifteenDouble + fifteenInt
Prelude>
```

********** BEGIN EXERCISES: TYPE MATCHING **********

Match the function to its type signature, then check your work in GHCi.

1. Functions:

    a) `not`
    b) `length`
    c) `concat`
    d) `head`
    e) (<)

2. Type signatures:

    a) `_ :: [a] -> a`
    b) `_ :: [[a]] -> [a]`
    c) `_ :: Bool -> Bool`
    d) `_ :: [a] -> Int`
    e) `_ :: Ord a => a -> a -> Bool`

__________

2a) -> 1d)
2b) -> 1c)
2c) -> 1a)
2d) -> 1b)
2e) -> 1e)

```haskell
-- CORRECT
Prelude> :type not
not :: Bool -> Bool
-- CORRECT
Prelude> :type length
length :: Foldable t => t a -> Int
-- CORRECT (?)
Prelude> :type concat
concat :: Foldable t => t [a] -> [a]
-- CORRECT (?)
Prelude> :type head
head :: [a] -> a
-- CORRECT
Prelude> :type (<)
(<) :: Ord a => a -> a -> Bool
Prelude>
```

********** END EXERCISES: TYPE MATCHING **********
