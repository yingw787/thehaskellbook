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
