# Chapter 7

- More functional patterns
    - Functions are first-class values in Haskell
    - First class value: Values that can be used as arguments to a function

- Setting parameters
    - Functions differ from values because they apply parameters to arguments

```haskell
Prelude> myNum :: Integer; myNum = 1
Prelude> myVal = myNum
Prelude> :t myVal
myVal :: Integer
Prelude> myVal f = myNum
Prelude> :t myVal
myVal :: p -> Integer
Prelude> myVal f = f + myNum
Prelude> :t myVal
myVal :: Integer -> Integer
Prelude>
```

- Applying more parameters results in generating a partially applied (but still
  valid) function

```haskell
-- f g not used
Prelude> myVal f g = myNum
Prelude> :t myVal
myVal :: p1 -> p2 -> Integer
-- f g h not used
Prelude> myVal f g h = myNum
Prelude> :t myVal
myVal :: p1 -> p2 -> p3 -> Integer
Prelude>
```

- Scoping / binding variables to values
    - Applying a function binds parameters to values
    - Type parameters are bound to concrete types
    - Function variables are bound to concrete values

```haskell
Prelude> :{
Prelude| addOne :: Integer -> Integer
Prelude| addOne x = x + 1
Prelude| :}
Prelude> addOne 1
2
Prelude> addOne 1 = 1 + 1
-- An error occured when accepting to reuse method `addOne`, as explained here:
-- https://stackoverflow.com/a/26738476
--
-- Add to a file and :load into GHCi
--
-- Error may be because using single equals instead of double equals
Prelude> addOne 10
*** Exception: <interactive>:18:1-16: Non-exhaustive patterns in function addOne
```

See `scoping.hs`.

- Anonymous functions

```haskell
Prelude> :{
-- Normal named method
Prelude| triple :: Integer -> Integer
Prelude| triple x = x * 3
Prelude| :}
Prelude> :{
-- Anonymous / lambda method
Prelude| (\x -> x * 3) :: Integer -> Integer
Prelude| :}

-- Method does not implement
<interactive>:46:1: error:
    • No instance for (Show (Integer -> Integer))
        arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In a stmt of an interactive GHCi command: print it
Prelude> :{
-- Assigning a lambda method to a variable
Prelude| let trip :: Integer -> Integer
Prelude|     trip = \x -> x * 3
Prelude| :}
-- Need to wrap lambda methods in parentheses in order to apply to variables
Prelude> (\x -> x * 3) 5
15
-- Otherwise this happens
Prelude> \x -> x * 3 1

-- I get a different error than the book
--
-- But difficult to deduce that 1 is an argument and not part of the method
-- definition.
<interactive>:57:1: error:
    • Non type-variable argument in the constraint: Num (t -> a)
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        it :: forall a t. (Num a, Num t, Num (t -> a)) => a -> a
Prelude>
```

********** BEGIN EXERCISES: GRAB BAG **********

********** END EXERCISES: GRAB BAG **********
