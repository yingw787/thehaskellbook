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
