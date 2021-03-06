# Chapter 8

- Recursion: self-referential expressions

- Important because it allows us to define something indefinite, or incremental
  computation without having to duplicate source code.

- On the surface, lambda calculus doesn't seem to have a notion for recursion.
    - Being able to write recursive functions is essential to Turing completeness
        - Turing completeness: Can simulate a Turing machine (have some data
          representation, and some operations like reading, editing (writing or
          nullifying), and moving the tape)
    - Y combinator, or fixed point combinator, helps write recursive functions
      in the lambda syntax.

- Factorial

```haskell
Prelude> :{
Prelude| fourFactorial :: Integer
-- Only handles one exact case of factorial. impractical.
Prelude| fourFactorial = 4 * 3 * 2 * 1
Prelude| :}
Prelude> :{
Prelude| brokenFact1 :: Integer -> Integer
-- Broken, never stops because there's never a base case.
Prelude| brokenFact1 n = n * brokenFact1 (n - 1)
Prelude| :}
Prelude> :{
Prelude| factorial :: Integer -> Integer
-- Success, because now we have a base case at 0.
Prelude| factorial 0 = 1
Prelude| factorial n = n * factorial (n - 1)
Prelude| :}
Prelude> factorial 4
24
Prelude>
```

- Another way to look at recursion
    - Function composition (taking a function and passing it into another
      function) is like recursion (except recursion enforces that the same
      function gets passed around)
    - Function composition is definite, while recursion is indefinite.
    - A programming language built on top of lambda calculus has one verb for
      expressing computations that can b evaluated: *apply*. Everything else is
      pretty much syntactic sugar.

```haskell
Prelude> :{
-- Prelude> incTimes 10 0
-- 10
-- Prelude> incTimes 5 0
-- 5
-- Prelude> incTimes 5 5
-- 10
Prelude| incTimes :: (Eq a, Num a) => a -> a -> a
Prelude| incTimes 0 n = n
-- `times` is variable denoting number of times to call method.
Prelude| incTimes times n = 1 + (incTimes (times - 1) n)
Prelude| :}
Prelude> :{
Prelude| applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
Prelude| applyTimes 0 f b = b
Prelude| applyTimes n f b = f (applyTimes (n - 1) f b)
-- Prelude> incTimes' 0 10
-- 10
-- Prelude> incTimes' 10 0
-- 10
-- Prelude> incTimes' 5 5
-- 10
Prelude| incTimes' :: (Eq a, Num a) => a -> a -> a
Prelude| incTimes' times n = applyTimes times (+1) n
Prelude| :}
Prelude>
```

********** BEGIN INTERMISSION: EXERCISE **********

(+1) . applyTimes (4) (+1) $ 5
(+1) . (+1) . applyTimes (3) (+1) $ 5
(+1) . (+1) . (+1) . applyTimes (2) (+1) $ 5
(+1) . (+1) . (+1) . (+1) . applyTimes (1) (+1) $ 5
(+1) . (+1) . (+1) . (+1) . (+1) . applyTimes (0) (+1) $ 5
(+1) . (+1) . (+1) . (+1) . (+1) . 5
10

********** END INTERMISSION: EXERCISE **********

- Bottom
    - Refers to computations that do not successfully result in a value.
    - Denoted by `(⊥)`.
    - Mostly failed with error, or failed to terminate.
    - In logic, corresponds to False.

```haskell
Prelude> :{
Prelude| f :: Bool -> Int
-- Calling `True` returns in an exception, which is an example of `bottom`.
Prelude| f True = error "blah"
-- Calling `False` doesn't result in a bottom value.
Prelude| f False = 0
Prelude| :}
Prelude> f False
0
Prelude> f True
*** Exception: blah
CallStack (from HasCallStack):
  error, called at <interactive>:79:10 in interactive:Ghci20
Prelude> :{
-- `f'` is an example of a method that is not totally defined, which results
-- in an error when a valid typed input argument is called with no expected
-- return value.
Prelude| f' :: Bool -> Int
Prelude| f' False = 0
Prelude| :}
Prelude> f' False
0
Prelude> f' True
*** Exception: <interactive>:90:1-12: Non-exhaustive patterns in function f'

Prelude> :{
-- Method `f''` is equivalent to method `f'`.
Prelude| f'' :: Bool -> Int
Prelude| f'' False = 0
Prelude| f'' _ = error $ "*** Exception: Non-exhaustive patterns in function f"
Prelude| :}
Prelude> f'' False
0
Prelude> f'' True
*** Exception: *** Exception: Non-exhaustive patterns in function f
CallStack (from HasCallStack):
  error, called at <interactive>:97:9 in interactive:Ghci24
Prelude>
```

- See `brokenMaybe1.hs`.

- Fibonacci numbers

```haskell
Prelude> :{
Prelude| fibonacci :: Integral a => a -> a
Prelude| fibonacci 0 = 0
Prelude| fibonacci 1 = 1
Prelude| fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)
Prelude| :}
Prelude> fibonacci 6
8
Prelude>
```

- Integral division from scratch

```haskell
-- Weird indentation errors required by `go` statement. If it's not indented
-- correctly, then a compile-time error will result.
--
-- `go` functions enable us to define functions with a `where` clause that can
-- accept more arguments than top-level `dividedBy` function can. In this case,
-- we need to keep track of how manyt times we have subtracted the current
-- denominator from the current numerator.
--
-- Prelude> :{
-- Prelude| dividedBy :: Integral a => a -> a -> (a, a)
-- Prelude| dividedBy num denom = go num denom 0
-- Prelude|   where   go   n d count
-- Prelude|            | n < d = (count, n)
-- Prelude|            | otherwise = go (n - d) d (count + 1)
-- Prelude| :}
-- Prelude> dividedBy 10 5
-- (2,0)
-- Prelude> dividedBy 10 6
-- (1,4)
-- Prelude>
--
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where   go   n d count
           | n < d = (count, n)
           | otherwise = go (n - d) d (count + 1)
```

********** BEGIN CHAPTER EXERCISES **********

Review of types

(ALL CORRECT, ANSWER KEY https://github.com/johnchandlerburnham/hpfp)

1. d)
2. b)
3. d)
4. b)

Reviewing currying

```haskell
:{
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y
flippy = flip cattyConny
appedCatty = cattyConny "woops"
frappe = flippy "haha"
:}
```

1. `x + "woops mrow woohoo!"`

(CORRECT)

```haskell
Prelude> :{
Prelude| cattyConny :: String -> String -> String
Prelude| cattyConny x y = x ++ " mrow " ++ y
Prelude| flippy = flip cattyConny
Prelude| appedCatty = cattyConny "woops"
Prelude| frappe = flippy "haha"
Prelude| :}
Prelude> appedCatty "woohoo!"
"woops mrow woohoo!"
Prelude>
```

2. `"haha mrow 1"` (INCORRECT, `"1 mrow haha"`)

3. `"woops mrow 2 mrow haha"` (CORRECT)

4. `"woops mrow blue mrow haha"` (CORRECT)

5. `"pink mrow haha mrow green mrow woops mrow blue"` (CORRECT)

6. `"are mrow Pugs mrow awesome"` (CORRECT)

Recursion

1. Below:

```haskell
dividedBy 15 2
go 15 2 0
go 13 2 1
go 11 2 2
go 9 2 3
go 7 2 4
go 5 2 5
go 3 2 6
go 1 2 7
(7, 1)
```

2. Below:

```haskell
recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum 1 = 1
recursiveSum n = n + recursiveSum (n - 1)
```

(CORRECT, GHCI RESULTS BELOW)

```haskell
Prelude> :{
Prelude| recursiveSum :: (Eq a, Num a) => a -> a
Prelude| recursiveSum 1 = 1
Prelude| recursiveSum n = n + recursiveSum (n - 1)
Prelude| :}
Prelude> recursiveSum 5
15
Prelude>
```

3. Below:

```haskell
recursiveAdd :: (Integral a) => a -> a -> a
recursiveAdd x y = go x y 0
  where   go   a b count
           | a == 0 = count
           | otherwise = go (a - 1) b (count + b)
```

(CORRECT, GHCI RESULTS BELOW)

```haskell
Prelude> :{
Prelude| recursiveAdd :: (Integral a) => a -> a -> a
Prelude| recursiveAdd x y = go x y 0
Prelude|   where   go   a b count
Prelude|            | a == 0 = count
Prelude|            | otherwise = go (a - 1) b (count + b)
Prelude| :}
Prelude> recursiveAdd 7 8
56
Prelude> recursiveAdd 8 7
56
Prelude> recursiveAdd 9 9
81
Prelude>
```

Fixing dividedBy

See `dividedBy.hs`.

McCarthy 91 function

See `McCarthy91.hs`.

Numbers into words

See `NumbersIntoWords.hs`.

********** END CHAPTER EXERCISES **********
