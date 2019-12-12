# Chapter 10

- Folds
    - General concept called "catamorphisms"
    - Catamorphism: deconstructing data
    - If the spine of a list is the structure of a list, then a fold is what can
      reduce the structure.

- Bringing you into the fold

```haskell
-- GHC 7.10 abstracts out list-specific part of folding into a typeclass
-- (not `[]`.)
Prelude> :t foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
Prelude>
-- We can always recover the concrete type from the generic type. Vice versa not
-- possible.
Prelude> :{
Prelude| let listFoldr   :: (a -> b -> b)
Prelude|                 -> b
Prelude|                 -> [] a
Prelude|                 -> b
Prelude|     listFoldr = foldr
Prelude| :}
Prelude> :t listFoldr
listFoldr :: (a -> b -> b) -> b -> [a] -> b
Prelude>
```

- Some parallels between `map` and `foldr`
    - `map` applies a function to each member of a list and returns a list
    - `foldr` replaces the `cons` constructors with the function and reduces the
      list.

- Recursive patterns

```haskell
-- These reproductions of `sum`, `length`, `product`, and `concat` have the
-- base case be the identity of the function, and a main function that recurses
-- to the right. Head of list gets evaluated, set aside, and function moves to
-- the right and evaluates the next head.
Prelude> :{
Prelude| sum' :: [Integer] -> Integer
Prelude| sum' [] = 0
Prelude| sum' (x : xs) = x + sum' xs
Prelude|
Prelude| length' :: [a] -> Integer
Prelude| length' [] = 0
Prelude| length' (_ : xs) = 1 + length' xs
Prelude|
Prelude| product' :: [Integer] -> Integer
Prelude| product' [] = 1
Prelude| product' (x : xs) = x * product' xs
Prelude|
Prelude| concat' :: [[a]] -> [a]
Prelude| concat' [] = []
Prelude| concat' (x : xs) = x ++ concat' xs
Prelude| :}
Prelude>
```

- Fold right

```haskell
Prelude> :{
Prelude| foldr' :: (a -> b -> b) -> b -> [a] -> b
Prelude| foldr' f z [] = z
Prelude| foldr' f z (x : xs) = f x (foldr' f z xs)
Prelude| :}
Prelude> foldr' (+) 0 [1, 2, 3]
6
-- Here, we can see how `foldr''` or `foldr` is very similar to the
-- recursive patterns from earlier. There is a method, and an identity
-- value, evaluated to the right over a list.
--
-- Since `(+)` is strict in its arguments, it unconditionally forces the rest
-- of the fold.
--
Prelude> :{
Prelude| foldr'' :: (a -> b -> b) -> b -> [a] -> b
Prelude| foldr'' f z xs =
Prelude|     case xs of
Prelude|         [] -> z
Prelude|         (x : xs) -> f x (foldr'' f z xs)
Prelude| :}
-- The expression below could be rewritten as:
--
-- `(+) 1 ((+) 2 ((+) 3 0))`
-- `1 + (2 + (3 + 0))`
-- `1 + (2 + 3)`
-- `1 + 5`
-- `6`
--
Prelude> foldr'' (+) 0 [1, 2, 3]
6
Prelude>
```

```haskell
-- This is a trick to show how `foldr` associates.
Prelude> map show [1..5]
["1","2","3","4","5"]
Prelude> xs = map show [1..5]
Prelude> foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" xs
"(1+(2+(3+(4+(5+0)))))"
Prelude>
```

- Folding occurs in two stages: traversal and folding.
    - Traversal: fold recurses over the spine.
    - Folding: Reduction of folding function applied over the values.

```haskell
Prelude> :{
Prelude| foldr'' :: (a -> b -> b) -> b -> [a] -> b
Prelude| foldr'' f z xs =
Prelude|     case xs of
Prelude|         [] -> z
-- If method `f` doesn't evaluate the second argument, then the spine will
-- not be forced.
--
-- This means`foldr` can avoid evaluating not only the values, but also the
-- spine. This means `foldr` can be used with lists that are potentially
-- infinite (e.g. stream processing).
--
-- There is no guarantee that `fold` on infinite list will finish evaluating
-- since it depends on the fold function and input data.
Prelude|         (x : xs) -> f x (foldr'' f z xs)
Prelude| :}
Prelude> :{
Prelude| myAny :: (a -> Bool) -> [a] -> Bool
Prelude| myAny f xs =
Prelude|     foldr'' (\x b -> f x || b) False xs
Prelude| :}
-- `[1..]` is an infinite list, yet `myAny` doesn't fail because it returns
-- immediately if a value evaluates to True.
Prelude> myAny even [1..]
True
Prelude>
```

```haskell
Prelude> u = undefined
-- Here, "bottom" is part of the value construction.
Prelude> foldr (+) 0 [1, 2, 3, 4, u]
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:111:5 in interactive:Ghci17
Prelude> xs = take 4 [1, 2, 3, 4, u]
-- `foldr` doesn't evaluate the value if it is not forced.
Prelude> foldr (+) 0 xs
10
Prelude>
-- Here, "bottom" is part of the spine as well.
Prelude> xs' = [1, 2, 3, 4] ++ u
Prelude> foldr (+) 0 xs'
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:111:5 in interactive:Ghci17
Prelude> xs'' = take 4 ([1, 2, 3, 4] ++ u)
-- `foldr` also doesn't evaluate the spine if not needed.
Prelude> foldr (+) 0 xs''
10
-- Method `length` is different, since it evaluates the spine only
Prelude> length [1, 2, 3, 4, u]
5
-- If "bottom" is part of the spine, then `length` is forced to raise
-- an exception.
Prelude> length ([1, 2, 3, 4] ++ u)
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:111:5 in interactive:Ghci17
-- Method `take` is non-strict, and stops returning elements of list upon
-- hitting length limit given.
Prelude> length (take 4 xs')
4
Prelude>
```

```haskell
Prelude> xs = [1, 2] ++ undefined
-- Method `take 4` would hit bottom, but doesn't matter due to `take 2`
-- in between.
Prelude> length $ take 2 $ take 4 xs
2
Prelude>
```

```haskell
-- Assume an anonymous method that ignores all inputs and just returns 9001.
--
-- This method never forces evaluation of any of its argument.
Prelude> foldr (\_ _ -> 9001) 0 [1..5]
9001
Prelude> foldr (\_ _ -> 9001) 0 [1, 2, 3, undefined]
9001
Prelude> foldr (\_ _ -> 9001) 0 ([1, 2, 3] ++ undefined)
9001
-- Only if the first `cons` cell is "bottom", does the method raise an
-- exception.
Prelude> foldr (\_ _ -> 9001) 0 undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:132:24 in interactive:Ghci32
-- This input argument works b ecause it isn't the first `cons cell` that is
-- bottom. The undefined values are inside the cons cell, not the spine
-- itself. The cons cells "contain" bottom values but are not themselves
-- bottom.
Prelude> foldr (\_ _ -> 9001) 0 [undefined, undefined]
9001
Prelude>
```

- Fold left

```haskell
-- (I don't think you need to import `Data.List` for GHCi v8.4.3)
Prelude> map show [1..5]
["1","2","3","4","5"]
Prelude> xs = map show [1..5]
Prelude> foldl (\x y -> concat ["(", x, "+", y, ")"]) "0" xs
"(((((0+1)+2)+3)+4)+5)"
Prelude>
```

- `scans` can also see how folds evaluate.
    - Scans return a list of all the intermediate stages of the fold.
    - `scanl` and `scanr` map to `foldl` and `foldr` respectively.

```haskell
Prelude> foldr (+) 0 [1..5]
15
-- head (scanr f z xs) = foldr f z xs
Prelude> scanr (+) 0 [1..5]
[15,14,12,9,5,0]
Prelude> foldl (+) 0 [1..5]
15
-- last (scanl f z xs) = foldl f z xs
Prelude> scanl (+) 0 [1..5]
[0,1,3,6,10,15]
Prelude>
```

- Associativity and folding
    - `foldl` and `foldr` differ in associativity in evaluation.

```haskell
-- Here demonstrated with a non-commutative operation `(-)`.
Prelude> foldl (\x y -> concat ["(", x, "-", y, ")"]) "0" xs
"(((((0-1)-2)-3)-4)-5)"
Prelude> foldr (\x y -> concat ["(", x, "-", y, ")"]) "0" xs
"(1-(2-(3-(4-(5-0)))))"
Prelude> foldl (-) 0 [1..5]
-15
Prelude> foldr (-) 0 [1..5]
3
Prelude>
```

```haskell
Prelude> foldr (:) [] [1..3]
[1,2,3]
-- Must use `flip` with `foldl`; because `(:)` dictates value be the first
-- argument and the list the second argument. Value must be prepended to the
-- front of the list.
--
-- This still results in a different association than `foldr`, as can be seen
-- from the result.
Prelude> foldl (flip (:)) [] [1..3]
[3,2,1]
Prelude> foldl (:) [] [1..3]

<interactive>:21:7: error:
    • Occurs check: cannot construct the infinite type: a ~ [a]
      Expected type: [a] -> [a] -> [a]
        Actual type: a -> [a] -> [a]
    • In the first argument of ‘foldl’, namely ‘(:)’
      In the expression: foldl (:) [] [1 .. 3]
      In an equation for ‘it’: it = foldl (:) [] [1 .. 3]
    • Relevant bindings include it :: [a] (bound at <interactive>:21:1)
Prelude>
```

```haskell
-- `const` takes two arguments and returns the first one.
--
-- Here, 0 is the identity value returned at the end of spine recursion,
-- and 1 is the first value. Folding stops after 1 and 2 are fetched.
--
-- `(1 `const` (2 `const` (3 `const` (4 `const` (5 `const` 0)))))`
-- `(1 `const` (2 `const` (3 `const` (4 `const` 5))))`
-- `(1 `const` (2 `const` (3 `const` 4)))`
-- `(1 `const` (2 `const` 3))`
-- `(1 `const` 2)`
-- `1`
Prelude> foldr const 0 [1..5]
1
-- Here, 0 is the accumulator value.
--
-- `(1 `flip const` (2 `flip const` (3 `flip const` (4 `flip const` (5 `flip const` 0)))))`
-- `(1 `flip const` (2 `flip const` (3 `flip const` (4 `flip const` 0))))`
-- `(1 `flip const` (2 `flip const` (3 `flip const` 0)))`
-- `(1 `flip const` (2 `flip const` 0))`
-- `(1 `flip const` 0)`
-- `0`
Prelude> foldr (flip const) 0 [1..5]
0
-- `(((((0 `const` 1) `const` 2) `const` 3) `const` 4) `const` 5)`
-- `((((0 `const` 2) `const` 3) `const` 4) `const` 5)`
-- `(((0 `const` 3) `const` 4) `const` 5)`
-- `((0 `const` 4) `const` 5)`
-- `(0 `const` 5)`
-- `0`
Prelude> foldl const 0 [1..5]
0
-- `(((((0 `flip const` 1) `flip const` 2) `flip const` 3) `flip const` 4) `flip const` 5)`
-- `((((1 `flip const` 2) `flip const` 3) `flip const` 4) `flip const` 5)`
-- `(((2 `flip const` 3) `flip const` 4) `flip const` 5)`
-- `((3 `flip const` 4) `flip const` 5)`
-- `(4 `flip const` 5)`
-- `5`
Prelude> foldl (flip const) 0 [1..5]
5
Prelude>
```

********** BEGIN EXERCISES: UNDERSTANDING FOLDS **********

1. c), `(*)` is a commutative operator.

(CORRECT)

```haskell
Prelude> foldr (*) 1 [1..5]
120
Prelude> foldl (*) 1 [1..5]
120
Prelude>
```

2. Below:

```haskell
-- `foldl (flip (*)) 1 [1..3]`
-- `(((1 * 1) * 2) * 3)`
-- `((1 * 2) * 3)`
-- `(2 * 3)`
-- `6`
--
-- (CORRECT, GHCI RESULTS BELOW)
Prelude> foldl (flip (*)) 1 [1..3]
6
Prelude>
```

3. c)

4. a)

5. See `UnderstandingFolds.hs`.

********** END EXERCISES: UNDERSTANDING FOLDS **********

- Unconditional spine recursion
    - `foldl` has the successive steps of fold as first argument; next recursion
      is not intermediated by the folding function as it is in `foldr`, which
      means recursion of the spine is unconditional.
    - `foldl` is fine with non-strict evaluation of values, as long as the fold
      function is also non-strict (e.g. `(\_ _ = 5)`).

```haskell
-- Include "bottom" as part of the spine of list `xs`.
Prelude> xs = [1..5] ++ undefined
-- Non-strict evaluation over both spine/values means only the first value of
-- `xs` is fetched, and rest of `xs` is not evaluated.
Prelude> foldr const 0 xs
1
-- "Bottom" is fetched by value, which results in an exception.
Prelude> foldr (flip const) 0 xs
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:1:16 in interactive:Ghci1
-- `foldl` is left-associative, which means it traverses the entire spine.
Prelude> foldl const 0 xs
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:1:16 in interactive:Ghci1
-- Doesn't matter if `flip` is applied, `xs` is still traversed in full.
Prelude> foldl (flip const) 0 xs
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:1:16 in interactive:Ghci1
Prelude>
```

- This means `foldl` is generally inappropriate with streams or long lists.
- In most cases, when needing a left fold, use `fold'`, or `fold-l-prime`, which
  is strict for both spine/values.
    - Apparently this has less negative effect on performance over long lists.

- How to write fold functions
    - Start value: identity value for the function (0, 1, []).
    - Then consider arguments for folding function
    - Then consider folding function itself.

********** START EXERCISES: DATABASE PROCESSING **********

See `DatabaseProcessing.hs`.

********** END EXERCISES: DATABASE PROCESSING **********

- Folding and evaluation

```haskell
-- Relationship between `foldr` and `foldl`.
--
-- Only for finite lists!
foldr f z xs = foldl (flip f) z (reverse xs)
```

- Summary

- Scans

```haskell
Prelude> scanr (+) 0 [1..3]
-- [1 + (2 + (3 + 0)), 2 + (3 + 0), 3 + 0, 0]
[6,5,3,0]
Prelude> scanl (+) 0 [1..3]
-- [0, 0 + 1, 0 + 1 + 2, 0 + 1 + 2 + 3]
[0,1,3,6]
Prelude>
```

```haskell
-- Older definition of method `scanl`.
Prelude> :{
Prelude| scanl' :: (a -> b -> a) -> a -> [b] -> [a]
Prelude| scanl' f q ls =
Prelude|   q : (case ls of
Prelude|         [] -> []
Prelude|         x : xs -> scanl f (f q x) xs)
Prelude| :}
Prelude>
```

```haskell
-- Stream of fibanocci numbers.
Prelude> fibs = 1 : scanl (+) 1 fibs
-- Get the Nth fibanocci number out of the stream.
Prelude> fibsN x = fibs !! x
Prelude> fibsN 0
1
Prelude> fibsN 2
2
Prelude> fibsN 6
13
Prelude>
```

********** BEGIN SCANS EXERCISES **********

1. Below:

```haskell
Prelude> fibs = take 20 $ 1 : scanl (+) 1 fibs
Prelude> fibs
[1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765]
Prelude> length fibs
20
Prelude>
```

2. Below:

```haskell
-- (Personal note: filtering on an infinite stream results in bottom due to
-- a lack of process termination.)
Prelude> fibs = filter (\x -> x < 100) (1 : scanl (+) 1 fibs)
Prelude> fibs
[1,1,2,3,5,8,13,21,34,55,89

^CInterrupted.
-- This isn't it either.
Prelude> fibs = 1 : (\x -> if x < 100 then scanl (+1) 1 fibs else 0)

<interactive>:73:13: error:
    • Couldn't match expected type ‘[a]’
                  with actual type ‘Integer -> [Integer]’
    • The lambda expression ‘\ x -> ...’ has one argument,
      but its type ‘[a]’ has none
      In the second argument of ‘(:)’, namely
        ‘(\ x -> if x < 100 then scanl (+ 1) 1 fibs else 0)’
      In the expression:
        1 : (\ x -> if x < 100 then scanl (+ 1) 1 fibs else 0)
    • Relevant bindings include
        fibs :: [a] (bound at <interactive>:73:1)

<interactive>:73:42: error:
    • Couldn't match type ‘Integer’ with ‘a -> Integer’
      Expected type: Integer -> a -> Integer
        Actual type: (a -> Integer) -> a -> Integer
    • In the first argument of ‘scanl’, namely ‘(+ 1)’
      In the expression: scanl (+ 1) 1 fibs
      In the expression: if x < 100 then scanl (+ 1) 1 fibs else 0
    • Relevant bindings include
        fibs :: [a] (bound at <interactive>:73:1)
Prelude>
-- FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp
-- (Use method `takeWhile` to naturally terminate this case. I forgot
-- this method existed.)
Prelude> fibs' n = takeWhile (< n) (1 : scanl (+) 1 fibs)
Prelude> fibs' 100
[1,1,2,3,5,8,13,21,34,55,89]
Prelude>
```

3. Below:

```haskell
-- BLANKING NOT SURE
--
-- Prelude> factorial = 1 : scanl (*) 2 factorial
-- Prelude> factorial = scanl (*) 2 factorial
-- Prelude> factorial = scanl (*) 1 factorial
-- FROM ANSWER KEY
Prelude> factorial = scanl (*) 1 [1..]
```

********** END SCANS EXERCISES **********
