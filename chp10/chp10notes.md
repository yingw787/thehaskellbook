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
