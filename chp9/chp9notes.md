# Chapter 9

- Lists
    - Refer to and process a collection or plurality of values.
    - **Infinite series of values, which acts as a stream datatype.** (PERSONAL
      NOTE: Nice, I don't think Python has this feature)

- The list datatype

```haskell
data [] a = [] | a : [a]
```

- `(:)` is the "cons" operator (short for construct)
- List as a whole is a sum type (sum of data), but can be constructed as a
  product (adding single element to list and getting a list back)

- Pattern matching on lists

```haskell
-- Get the head of a list (first element)
Prelude> myHead (x : _) = x
Prelude> :t myHead
myHead :: [a] -> a
Prelude> myHead [1, 2, 3]
1
-- Get the tail of a list (everything except the first element)
Prelude> myTail (_ : xs) = xs
Prelude> :t myTail
myTail :: [a] -> [a]
Prelude> myTail [1, 2, 3]
[2,3]
-- Neither method `myHead` nor method `myTail` will match on an empty list.
-- Type signature `[a]` doesn't guarantee that it will contain at least one
-- `a` value.
Prelude> myHead []
*** Exception: <interactive>:1:1-18: Non-exhaustive patterns in function myHead

Prelude> myTail []
*** Exception: <interactive>:4:1-20: Non-exhaustive patterns in function myTail

Prelude> :{
-- Method `myTail` can be reinforced by adding a base case condition for
-- an empty list.
Prelude| myTail' :: [a] -> [a]
Prelude| myTail' [] = []
Prelude| myTail' (_ : xs) = xs
Prelude| :}
Prelude> myTail' [1..5]
[2,3,4,5]
Prelude> myTail' []
[]
-- The `Maybe` data constructor may be able to help build safer functions.
Prelude> :info Maybe
data Maybe a = Nothing | Just a 	-- Defined in ‘GHC.Base’
-- Additional output elided
Prelude> :{
-- Define a safe method using `Maybe` in order to get the tail of a list under
-- all conditions.
Prelude| safeTail :: [a] -> Maybe [a]
Prelude| safeTail [] = Nothing
Prelude| safeTail (_:[]) = Nothing
Prelude| safeTail (_:xs) = Just xs
Prelude| :}
Prelude> safeTail [1]
Nothing
Prelude> safeTail []
Nothing
Prelude> safeTail [1,2,3]
Just [2,3]
Prelude>
```

- List's syntactic sugar

```haskell
-- Syntactic sugar for operator `cons`.
Prelude> [1, 2, 3] ++ [4]
[1,2,3,4]
-- The underlying, desugared command.
--
-- The `cons` constructor, or "cons cell" `a : [a]` at the very end of this
-- list is the conceptual space that values may inhabit.
--
-- The "spine" is the connective structure that holds the cons cells
-- together and in place
Prelude> (1 : 2 : 3 : []) ++ 4 : []
[1,2,3,4]
Prelude>
```

- Using ranges to construct lists

```haskell
-- Range syntax (syntactic sugar)
Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]
-- Desugared equivalent using `Enum` methods.
Prelude> enumFromTo 1 10
[1,2,3,4,5,6,7,8,9,10]
Prelude> [1,2..10]
[1,2,3,4,5,6,7,8,9,10]
Prelude> enumFromThenTo 1 2 10
[1,2,3,4,5,6,7,8,9,10]
Prelude> [1,3..10]
[1,3,5,7,9]
Prelude> enumFromThenTo 1 3 10
[1,3,5,7,9]
Prelude> [2, 4..10]
[2,4,6,8,10]
Prelude> enumFromThenTo 2 4 10
[2,4,6,8,10]
Prelude> ['t'..'z']
"tuvwxyz"
Prelude> enumFromTo 't' 'z'
"tuvwxyz"
Prelude>
```

```haskell
-- Enumeration methods
--
-- Methods `enumFrom` and `enumFromThen` can create infinitely long lists.
-- Should be ranging over type that has no upper bound in enumeration.
Prelude> :t enumFrom
enumFrom :: Enum a => a -> [a]
Prelude> :t enumFromThen
enumFromThen :: Enum a => a -> a -> [a]
-- Method `enumFromTo` should have first argument lower than second argument
-- otherwise an empty list will be returned.
Prelude> :t enumFromTo
enumFromTo :: Enum a => a -> a -> [a]
Prelude> :t enumFromThenTo
enumFromThenTo :: Enum a => a -> a -> a -> [a]
Prelude>
```

********** BEGIN EXERCISE: ENUMFROMTO **********

See `enumFromTo.hs`.

********** END EXERCISE: ENUMFROMTO **********

- Extracting portions of lists

```haskell
-- `take` takes the specified number of elements out of a list and returns
-- a list containing those elements.
--
-- take 7 ['a'..'z']
-- "abcdefg"
-- take 0 ['a'..'z']
-- ""
-- take 0 []
-- []
--
Prelude> :i take
take :: Int -> [a] -> [a] 	-- Defined in ‘GHC.List’
-- `drop` drops the specified number of elements off the beginning of the
-- list.
--
-- take 0 []
-- []
-- drop 5 [1..10]
-- [6,7,8,9,10]
-- drop 0 []
-- []
-- drop 5 []
-- []
--
Prelude> :i drop
drop :: Int -> [a] -> [a] 	-- Defined in ‘GHC.List’
-- `splitAt` cuts a list into two parts at the index specified by `Int` and
-- makes a tuple of two lists.
--
-- splitAt 5 [1..10]
-- ([1,2,3,4,5],[6,7,8,9,10])
-- splitAt 5 [1..6]
-- ([1,2,3,4,5],[6])
-- splitAt 5 []
-- ([],[])
--
Prelude> :i splitAt
splitAt :: Int -> [a] -> ([a], [a]) 	-- Defined in ‘GHC.List’
Prelude>
```

```haskell
-- Methods `takeWhile` and `dropWhile` are higher-order functions that accept
-- a predicate
-- (https://en.wikipedia.org/wiki/Predicate_%28mathematical_logic%29) and a
-- list together.
--
-- takeWhile (<3) [1..10]
-- [1,2]
-- takeWhile (<3) ['a'..'z']

-- <interactive>:50:13: error:
--     • No instance for (Num Char) arising from the literal ‘3’
--     • In the second argument of ‘(<)’, namely ‘3’
--       In the first argument of ‘takeWhile’, namely ‘(< 3)’
--       In the expression: takeWhile (< 3) ['a' .. 'z']
-- takeWhile (< 'j') ['a'..'z']
-- "abcdefghi"
-- takeWhile (> 6) [1..10]
-- [] -- fails upon first match.
-- takeWhile (== 'a') "abracadabra"
-- "a" -- matches only the first 'a' encountered.
-- takeWhile (> 6) [11..] -- Does not stop.
--
Prelude> :t takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
-- Method `dropWhile` is like `takeWhile`, except that it drops instead of
-- takes.
Prelude> :t dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
Prelude>
```

********** BEGIN EXERCISE: THY FEARFUL SYMMETRY **********

See `ThyFearfulSymmetry.hs`.

********** END EXERCISE: THY FEARFUL SYMMETRY **********

- List comprehensions
    - Come directly from set comprehensions in mathematics
    - One list (called the "generator") that gives input for comprehension

```haskell
Prelude> [x ^ 2 | x <- [1..10]]
--        0     1      3
-- 0: Function to apply transformation
-- 1: Separation between output function and input.
-- 2: Input set: generator list and variable representing variables drawn
-- from that list.
[1,4,9,16,25,36,49,64,81,100]
Prelude>
```

- Adding predicates
    - Can filter elements drawn from the generator list.
    - Predicates must evaluate to values of type `Bool`. Values passed to output
      function will only be those that met `True` case of predicate.

```haskell
-- Only square and output values that are originally even.
Prelude> [x ^ 2 | x <- [1..10], rem x 2 == 0]
[4,16,36,64,100]
Prelude>
```

```haskell
Prelude> :{
Prelude| [x ^ y |
-- Usage of multiple generator lists.
Prelude|   x <- [1..10],
Prelude|   y <- [2, 3],
-- Condition to return values less than 200.
Prelude|   x ^ y < 200]
Prelude| :}
[1,1,4,8,9,27,16,64,25,125,36,49,64,81,100]
Prelude>
```

```haskell
Prelude> :{
-- Executing effectively what is a cross-multiply, or a cartesian join, between
-- two separate generator lists.
--
-- Lists don't need to be the same length, or since tuples can be a collection
-- of types, even the same type.
Prelude| [
Prelude|   (x, y) |
Prelude|   x <- [1, 2, 3],
Prelude|   y <- [6, 7]
Prelude| ]
Prelude| :}
[(1,6),(1,7),(2,6),(2,7),(3,6),(3,7)]
Prelude> :{
Prelude| [
Prelude|   (x, y) |
Prelude|   x <- [1, 2, 3],
Prelude|   y <- ['a', 'b']
Prelude| ]
Prelude| :}
[(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
Prelude>
```

- You can use variable assignment to assign the result of a list comprehension
  to a new variable.

```haskell
Prelude> mySqr = [x ^ 2 | x <- [1..10]]
Prelude> :{
Prelude| [
Prelude|   (x, y) |
Prelude|   x <- mySqr,
Prelude|   y <- [1..3],
Prelude|   x < 4
Prelude| ]
Prelude| :}
[(1,1),(1,2),(1,3)]
Prelude> mySqr
[1,4,9,16,25,36,49,64,81,100]
Prelude>
```

********** BEGIN EXERCISE: COMPREHEND THY LISTS **********

1. Direct mapping of `mySqr`, filtering for values that are even only.

`[4,16,36,64,100]`

(CORRECT)

```haskell
Prelude> [x | x <- mySqr, rem x 2 == 0]
[4,16,36,64,100]
Prelude>
```

2. `[]`, double filter with same pivot value means no values will be ingested.

(INCORRECT, filters work on different sets of data, which can then be joined
together.)

```haskell
Prelude> :{
Prelude| [
Prelude|   (x, y) |
Prelude|   x <- mySqr,
Prelude|   y <- mySqr,
Prelude|   x < 50,
Prelude|   y > 50
Prelude| ]
Prelude| :}
[(1,64),(1,81),(1,100),(4,64),(4,81),(4,100),(9,64),(9,81),(9,100),(16,64),(16,81),(16,100),(25,64),(25,81),(25,100),(36,64),(36,81),(36,100),(49,64),(49,81),(49,100)]
```

3. `[(1,64),(1,81),(1,100),(4,64),(4,81)]`

(CORRECT)

```haskell
Prelude> thing = [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
Prelude> take 5 thing
[(1,64),(1,81),(1,100),(4,64),(4,81)]
Prelude>
```

********** END EXERCISE: COMPREHEND THY LISTS **********

- List comprehensions with Strings
    - Strings are lists, so you can execute string comprehensions.

```haskell
-- Method `elem` returns `Bool`: An element is present in a list of those
-- elements.
--
-- Type signature implies not just Char and String types.
Prelude> :t elem
elem :: (Foldable t, Eq a) => a -> t a -> Bool
Prelude> elem 'a' "abracadabra"
True
Prelude> elem 'a' "Julie"
False
relude> :{
Prelude| [
Prelude|   x |
Prelude|   x <- "Three Letter Acronym",
Prelude|   elem x ['A'..'Z']
Prelude| ]
Prelude| :}
"TLA"
Prelude> let acro xs = [x | x <- xs, elem x ['A'..'Z']]
Prelude> acro "Hello My Name Is"
"HMNI"
Prelude>
```

I think the function `myString xs = [x | x <- xs, elem x "aeiou"]`

********** BEGIN EXERCISE: SQUARE CUBE **********

```haskell
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
```

1. Below:

```haskell
:{
[
    (x, y) |
    x <- mySqr,
    y <- myCube
]
:}
```

(CORRECT, GHCI RESULTS BELOW)

```haskell
Prelude> :{
Prelude| [
Prelude|     (x, y) |
Prelude|     x <- mySqr,
Prelude|     y <- myCube
Prelude| ]
Prelude| :}
[(1,1),(1,8),(1,27),(1,64),(1,125),(4,1),(4,8),(4,27),(4,64),(4,125),(9,1),(9,8),(9,27),(9,64),(9,125),(16,1),(16,8),(16,27),(16,64),(16,125),(25,1),(25,8),(25,27),(25,64),(25,125)]
Prelude>
```

2. Below:

```haskell
:{
[
    (x, y) |
    x <- mySqr,
    y <- myCube,
    x < 50,
    y < 50
]
:}
```

(CORRECT, GHCI RESULTS BELOW)

```haskell
Prelude> :{
Prelude| [
Prelude|     (x, y) |
Prelude|     x <- mySqr,
Prelude|     y <- myCube,
Prelude|     x < 50,
Prelude|     y < 50
Prelude| ]
Prelude| :}
[(1,1),(1,8),(1,27),(4,1),(4,8),(4,27),(9,1),(9,8),(9,27),(16,1),(16,8),(16,27),(25,1),(25,8),(25,27)]
Prelude>
```

3. Below:

```haskell
-- ASSUMED CORRECT, applied `length` to list comprehension assigned to variable.
Prelude> thing = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
Prelude> length thing
15
Prelude>
```

********** END EXERCISE: SQUARE CUBE **********

- Spines and non-strict evaluation
    - When we talk about data structures in Haskell (esp. lists, sequences,
      trees), we talk about them having a "spine".
    - Spine: connective structure that ties the collection of values together.
    - For a list, a spine is textually represented by the recursive cons `(:)`
      operator.

```haskell
-- `cons` tree representation:
--
--   :
--  / \
-- 1   :
--    / \
--   2   []
--
Prelude> 1 : (2 : [])
[1,2]
Prelude>
```

- The `1` value does not exist *before* the `cons` cell, as the `cons` cell
  contains the `1` value.
- You can evaluate `cons` cells independently of what they contain.
- It is possible to evaluate the spine of the list without evaluating individual
  values.
- It is also possible to evaluate only part of the spine of a list and not the
  rest of it.

- Evaluating list in `cons` representation proceeds down the spine.
- Constructing the list proceeds up the spine.

- Haskell applies non-strict evaluation, so the list isn't constructed until
  it's consumed.
    - Until value is consumed, a series of placeholders as a blueprint of the
      list can be constructed.

```haskell
--
--   : <------|
--  / \       |
-- _   : <----| This is the "spine"
--    / \     |
--   _   : <--|
--      / \
--     _   []
--
```

- Using GHCi's `:sprint` command

- Print variables and see what is evaluated already and what hasn't
    - GHC Haskell has some opportunistic optimizations which introduce
      strictness to make code faster when it won't change how your code
      evaluates.

```haskell
Prelude> blah = enumFromTo 'a' 'z'
Prelude> :sprint blah
-- Value `blah` has not been evaluated yet.
blah = _
Prelude> take 1 blah
"a"
Prelude> :sprint blah
-- Only the first value has been evaluated.
blah = 'a' : _
Prelude> take 2 blah
"ab"
Prelude> :sprint blah
-- The second value has been evaluated; the first value was evaluated
-- from earlier.
blah = 'a' : 'b' : _
Prelude>
```

```haskell
-- Method `length` is only strict in the spine, evaluates the spine of the
-- list and not the values. However, GHCi may force `:sprint` to behave as
-- though we had forced evaluation of the values as well.
```

- Spines are evaluated independently of values
    - Weak head normal form: Contains possibility expression is fully evaluated
      (normal form), and possibility that expression has been evaluated to the
      point of arriving at a data constructor or lambda awaiting an argument.

```haskell
Prelude> (1, 2)
-- WHNF && NF
(1,2)
Prelude> (1, 1 + 1)
-- WHNF, Not NF, as operand `(+)` could be applied to arguments but
-- hasn't been yet.
(1,1+1)
-- WHNF && NF, as operand `(*)` has been applied to two arguments, but
-- cannot be further reduced until variable `x` has been applied to function.
Prelude> f = \x -> x * 10
-- Not WHNF and not NF, arguments are fully applied, but hasn't been evaluated.
-- Is not currently a data constructor.
Prelude> "Papu" ++ "chon"
-- WHNF, is a data constructor.
Prelude> (1, "Papu" ++ "chon")
```

```haskell
-- `num` is in normal form; all values are known and listed. Nothing left
-- to evaluate.
Prelude> num :: [Int]; num = [1, 2, 3]
Prelude> :sprint num
-- (PERSONAL NOTE: Hmm, this is weird. Book example gives num = [1,2,3]
-- indicating that variable `num` has been fully evaluated.)
num = _
Prelude> myNum :: [Int]; myNum = [1..10]
Prelude> :sprint myNum
myNum = _
Prelude> take 2 myNum
[1,2]
Prelude> :sprint myNum
-- WHNF of variable `myNum`; only evaluated when needed.
myNum = 1 : 2 : _
Prelude>
```

- Pattern matching is strict by default
    - Pattern matching on `cons` cells can mean forcing spine strictness if your
      function doesn't stop recursing the list. (PERSONAL NOTE: HOO BOY I
      thought everything was lazy; that may not be the case, it may be nicer to
      cleanly separate out lazy and eager evaluation like in those Python
      libraries)

```haskell
-- Calling `length` on `undefined` causes GHCi to crash.
Prelude> length undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:145:8 in interactive:Ghci36
-- Calling `length` on a list with `undefined` in it does not cause GHCi to
-- crash.
--
-- `length` recurses the spine and counts how many `cons` cells there are, and
-- doesn't check the actual values.
Prelude> length [1, undefined]
2
Prelude> :{
-- We can define our own version of method `length` as the following:
Prelude| length' :: [a] -> Integer
Prelude| length' [] = 0
-- `_` ignores values and proceeds to evaluate along the spine only.
-- `_` is a special value, recognized by the compiler. You cannot bind to `_`.
Prelude| length' (_ : xs) = 1 + length' xs
Prelude| :}
Prelude> length' [1, undefined]
2
-- Lists are lazily constructed, which may encounter runtime errors when
-- incorrectly declared then consumed.
Prelude> x = [1] ++ undefined ++ [3]
Prelude> x
[1*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:7:12 in interactive:Ghci3
Prelude>
```

```haskell
-- Method `(+)` or `sum` is strict in both values and spine evaluation.
Prelude> :{
Prelude| mySum :: Num a => [a] -> a
Prelude| mySum [] = 0
Prelude| mySum (x : xs) = x + mySum xs
Prelude| :}
-- Evaluation of `mySum` will keep recursing along the spine and evaluating
-- individual values before going back up the spin and summing up
-- inhabitants.
--
-- 1 + (2 + (3 + (4 + (5 + 0))))
-- 1 + (2 + (3 + (4 + 5)))
-- 1 + (2 + (3 + 9))
-- 1 + (2 + 12)
-- 1 + 14
-- 15
--
Prelude> mySum [1..5]
15
Prelude>
```

********** BEGIN EXERCISES: BOTTOM MADNESS **********

Will it blow up?

1. Returns `'⊥'`, since the `y` value will be undefined at time of `x ^ y = 1 ^
   undefined`.

(CORRECT)

```haskell
Prelude> [x ^ y | x <- [1..5], y <- [2, undefined]]
[1,*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:1:32 in interactive:Ghci1
Prelude>
```

2. Returns `1`. List comprehension returns a lazily generated list, which even
   though fully rendered using the `$` operator, remains lazy until consumed.
   Since only the first value is consumed, which is valid, the invalid is never
   hit and no exception is raised.

(CORRECT)

```haskell
Prelude> take 1 $ [x ^ y | x <- [1..5], y <- [2, undefined]]
[1]
Prelude>
```

3. Returns `'⊥'`, since summation eagerly loads both the spine and the value.

(CORRECT)

```haskell
Prelude> sum [1, undefined, 3]
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:3:9 in interactive:Ghci2
Prelude>
```

4. Returns `3`, since `length` evaluates the spine only, and not individual
   values.

(CORRECT)

```haskell
Prelude> length [1, 2, undefined]
3
Prelude>
```

5. Returns `'⊥'`, since fully evaluating operation `[1, 2, 3] ++ undefined`
   results in an exception, before function `length` is executed, due to operand
   `$`.

(CORRECT)

```haskell
Prelude> length $ [1, 2, 3] ++ undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:5:23 in interactive:Ghci3
Prelude>
```

6. Returns `'⊥'`, since `filter` should be applying a values-based comparison to
   check whether the modulo of the number given 2 is 0, and hence all values in
   the list should be numeric types.

(INCORRECT, returns `[2]`) (PERSONAL NOTE: Not sure how it does this, whether it
sees a sequence and then skips over certain elements without even comparing
them.)

```haskell
Prelude> take 1 $ filter even [1, 2, 3, undefined]
[2]
Prelude>
```

7. Returns `[]`, since `undefined` is not even, and it should skip given result
   of 6), and no values match `even` so `[]` will be returned from `$`. Taking
   values from an empty list should return an empty list, and not fail.

(INCORRECT, returns `'⊥'`.)

```haskell
Prelude> take 1 $ filter even [1, 3, undefined]
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:7:29 in interactive:Ghci4
Prelude>
```

8. Returns `'⊥'`, since `odd` characters in the list sequence means evaluating
   the value at every index.

(INCORRECT, returns `[1]`.)

```haskell
Prelude> take 1 $ filter odd [1, 3, undefined]
[1]
Prelude>
```

9. Returns `[1, 2]`, given result of 8).

(CORRECT)

```haskell
Prelude> take 2 $ filter odd [1, 3, undefined]
[1,3]
Prelude>
```

10. Returns `'⊥'`, given actual return value of 8).

(CORRECT)

```haskell
Prelude> take 3 $ filter odd [1, 3, undefined]
[1,3*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:10:28 in interactive:Ghci6
Prelude>
```

__________

Is it normal form?

(ANSWER KEY https://github.com/johnchandlerburnham/hpfp)

1. NORMAL FORM (CORRECT)

2. WEAK HEAD NORMAL FORM (CORRECT)

3. WEAK HEAD NORMAL FORM (INCORRECT, neither)

4. NEITHER (CORRECT)

5. NEITHER (CORRECT)

6. WEAK HEAD NORMAL FORM (INCORRECT, neither)

7. WEAK HEAD NORMAL FORM (CORRECT)

********** END EXERCISES: BOTTOM MADNESS **********
