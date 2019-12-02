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

- Lambda calculus does not have support for methods taking multiple arguments.
  Instead, functions with multiple arguments are syntactic sugar for curried
  methods.
- Currying is nesting of multiple functions to allow the illusion of
  multiple-parameter functions.

```haskell
-- `(+)` is a curried method, because it looks like it accepts two arguments,
-- when in reality it applies to one argument, which returns another function
-- that accepts the next argument, which actually creates the return value.
Prelude> :type (+)
(+) :: Num a => a -> a -> a
Prelude>
```

- The type constructor for functions makes currying *default* in Haskell, as it
  is an infix operator and right-associative.

```haskell
Prelude> :type (+)
(+) :: Num a => a -> a -> a
-- associates to: `(+) :: Num a => a -> (a -> a)`
-- Parentheses here group parameters into argument / result, as every arrow
-- must have one argument and one result.
Prelude>
```

- Partial application: apply only some of a function's arguments. This enables
  function reuse and composition.

```haskell
-- Define a method `addStuff` that takes two arguments and returns the sum of
-- both arguments and 5.
Prelude> :{
Prelude| addStuff :: Integer -> Integer -> Integer
Prelude| addStuff a b = a + b + 5
Prelude| :}
-- Takes one argument, returns a function that takes one argument, which
-- returns one result.
Prelude> :type addStuff
addStuff :: Integer -> Integer -> Integer
-- The above is equivalent to `addStuff :: Integer -> (Integer -> Integer)`.
-- Resolving the latter leads to addTen given input is 5.
Prelude> addTen = addStuff 5
Prelude> :type addTen
addTen :: Integer -> Integer
Prelude> fifteen = addTen 5
Prelude> :type fifteen
fifteen :: Integer
-- `fifteen` is equal to `addStuff 5 5`, because `addTen` is equal to
-- `addStuff 5`.
Prelude> fifteen
15
Prelude> addTen 5
15
Prelude> addStuff 5 5
15
Prelude>
```

- Examining partial application with a function that is not commutative:

```haskell
Prelude> :{
Prelude| subtractStuff :: Integer -> Integer -> Integer
Prelude| subtractStuff x y = x - y - 10
Prelude| :}
Prelude> subtractOne = subtractStuff 1
Prelude> :type subtractOne
subtractOne :: Integer -> Integer
-- result is equivalent to `subtractStuff 1 11`.
Prelude> result = subtractOne 11
Prelude> result
-20
Prelude>
```

- Un-currying is possible by replacing two functions with a two-tuple.
- Method signature would change from `Num a => a -> a -> a` to `Num => (a, a) ->
  a`. Representing single argument as the overarching tuple that carries
  multiple arguments.
    - PERSONAL QUESTION: Since a tuple can only have one type, how does
      uncurrying with multiple arguments of multiple types work out?

```haskell
Prelude> :{
Prelude| nonsense :: Bool -> Integer
Prelude| nonsense True = 805
Prelude| nonsense False = 31337
Prelude| :}
Prelude> :{
Prelude| curriedFunction :: Integer -> Bool -> Integer
Prelude| curriedFunction i b = i + (nonsense b)
Prelude| :}
Prelude> :{
Prelude| uncurriedFunction :: (Integer, Bool) -> Integer
Prelude| uncurriedFunction (i, b) = i + (nonsense b)
Prelude| :}
Prelude> :{
Prelude| anonymous :: Integer -> Bool -> Integer
Prelude| anonymous = \i b -> i + (nonsense b)
Prelude| :}
Prelude> :{
Prelude| anonNested :: Integer -> Bool -> Integer
-- `anonNested` does not leverage automatic currying, all manual currying.
Prelude| anonNested = \i -> \b -> i + (nonsense b)
Prelude| :}
-- `curriedFunction`, `anonymous`, and `anonNested` are all the same function
-- giving the same result.
Prelude> curriedFunction 10 False
31347
Prelude> uncurriedFunction (10, False)
31347
Prelude> anonymous 10 False
31347
Prelude> anonNested 10 False
31347
Prelude>
```

- You can curry methods using `curry`, and uncurry methods using `uncurry`:s

```haskell
-- Below is eqivalent to (curry f) a b = f (a, b)
Prelude> curry f a b = f (a, b)
Prelude> :t curry
curry :: ((a, b) -> t) -> a -> b -> t
Prelude> :t fst
fst :: (a, b) -> a
Prelude> :t curry fst
curry fst :: t -> b -> t
Prelude> fst (1, 2)
1
Prelude> curry fst 1 2
1
-- Below is equivalent to (uncurry f) (a, b) = f a b
Prelude> uncurry f (a, b) = f a b
Prelude> :t uncurry
uncurry :: (t1 -> t2 -> t3) -> (t1, t2) -> t3
Prelude> :t (+)
(+) :: Num a => a -> a -> a
Prelude> (+) 1 2
3
Prelude> uncurry (+) (1, 2)
3
Prelude>
```

- Sectioning: partial application of infix operators, which has a special syntax
  and can result in different associativity behaviors during function
  application:

```haskell
Prelude> x = 5
Prelude> y = (2^)
Prelude> z = (^2)
-- (2^) 5 -> 2 ^ 5
Prelude> y x
32
-- (^2) 5 -> 5 ^ 2
Prelude> z x
25
-- Doesn't just work for numeric types. Associativity applies here too.
Prelude> celebrate = (++ " woot!")
Prelude> celebrate "naptime"
"naptime woot!"
Prelude> _celebrate = ("its " ++)
Prelude> _celebrate "naptime!"
"its naptime!"
Prelude>
-- You can apply sectioning to a prefix method by turning the prefix method
-- into an infix method using backticks.
Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]
Prelude> elem 9 [1..10]
True
Prelude> 9 `elem` [1..10]
True
Prelude> c = (`elem` [1..10])
Prelude> c 9
True
Prelude> c 25
False
Prelude>
```

********** BEGIN EXERCISES: TYPE ARGUMENTS **********

Given a function and its type, tell us what type results from applying some or
all of the arguments. You can check your work in the REPL like this:

```haskell
Prelude> f :: a -> a -> a -> a; f = undefined
Prelude> x :: Char; x = undefined
Prelude> :type f x
```

*You can check the types of things that aren't implemented yet*, as long as you
give GHCi *undefined* to bind the signature to.

1. If the type of `f` is `a -> a -> a -> a`, and the type of `x` is `Char`, then
   the type of `f x` is:

    a) `Char -> Char -> Char`
    b) `x -> x -> x -> x`
    c) `a -> a -> a`
    d) `a -> a -> a -> Char`

2. If the type of `g` is `a -> b -> c -> b`, then the type of `g 0 'c' "woot"`
   is:

    a) `String`
    b) `Char -> String`
    c) `Int`
    d) `Char`

3. If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of `h
   1.0 2` is:

    a) `Double`
    b) `Integer`
    c) `Integral b => b`
    d) `Num b => b`

    Note that because the type variables `a` and `b` are different, the compiler
    *must* assume that the types could be different.

4. If the type of `h` is `(Num a, Num b) => a -> b -> b`, then the type of `h 1
   (5.5 :: Double)` is:

    a) `Integer`
    b) `Fractional b => b`
    c) `Double`
    d) `Num b => b`

5. If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a`, then the type of
   `jackal "keyboard" "has the word jackal in it" is:

    a) `[Char]`
    b) `Eq b => b`
    c) `b -> [Char]`
    d) `b`
    e) `Eq b => b -> [Char]`

6. If the type of `jackal` is `(Ord a, Eq b) => a -> b -> a`, then the type of
   `jackal "keyboard"` is:

    a) `b`
    b) `Eq b => b`
    c) `[Char]`
    d) `b -> [Char]`
    e) `Eq b => b -> [Char]`

7. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of
   `kessel 1 2` is:

    a) `Integer`
    b) `Int`
    c) `a`
    d) `(Num a, Ord a) => a`
    e) `Ord a => a`
    f) `Num a => a`

8. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of
   `kessel 1 (2 :: Integer)` is:

    a) `(Num a, Ord a) => a`
    b) `Int`
    c) `a`
    d) `Num a => a`
    e) `Ord a => a`
    f) `Integer`

9. If the type of `kessel` is `(Ord a, Num b) => a -> b -> a`, then the type of
   `kessel (1 :: Integer) 2` is:

    a) `Num a => a`
    b) `Ord a => a`
    c) `Integer`
    d) `(Num a, Ord a) => a`
    e) `a`

********** END EXERCISES: TYPE ARGUMENTS **********
