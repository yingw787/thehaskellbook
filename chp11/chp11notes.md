# Chapter 11

- Algebraic datatypes
    - Writing your own datatypes can help you leverage Haskell's most powerful
      features: pattern matching, type checking, and type inference
    - Data type: enumeration of constructors that can have zero or more
      arguments
    - Sum types, product types, product types with record syntax, type aliases,
      `newtype`

- Data declarations review
    - Custom datatypes help you model your domain that solve your problem.

```haskell
-- Can't override native syntax `[]`, so using `:info` in order to view data
-- types.
Prelude> :i Bool
data Bool = False | True 	-- Defined at <interactive>:1:1
-- 0 1    2 3     4 5
--
-- 1: Type constructor with no arguments.
--
-- 3: Data constructor. It takes no arguments, so it is a nullary constructor.
-- Shows p in term-level code.
--
-- 4: Logical disjunction on what values can be part of that type.
Prelude> :i []
data [] a = [] | a : [a] 	-- Defined in ‘GHC.Types’
--   7      8      9
--
-- 7: Type constructor with an argument. `a` in this case is a polymorphic
-- type variable, so list argument can be different types.
--
-- 9: Data constructor that takes in two arguments: `a` and `[a]`.
```

- `Bool` is an enumeration of two possible constructors, each with zero arguments
- `[]` enumerates two possible constructors and one of them takes two arguments.

- `|`, or logical disjunction, represents a "sum type", a type with more than
  one constructor.

- Data and type constructors
    - Type constructors
        - Used at the type level, in type signatures, and typeclass declarations
          and instances
        - Static, resolve at compile time
    - Data constructors
        - Construct values at term level
    - Can make a distinction between constants and constructors.
        - Type/data constructors that don't take arguments are constants.
        - `Bool` is a constant.
        - `True` and `False` are constants.
        - They're less constructors because they're not constructing anything.
        - Sometimes, a constructor may need to be parametrized with different
          types or amounts of data.
        - Then, they become like functions that must be applied to become a
          "concrete type/value".

```haskell
Prelude> data Trivial = Trivial'
--            0         1
-- 0: Type constructor is like a constant value at the type level.
-- Otherwise called type constants.
--
-- 1: Data constructor is like constant value at the value/term/runtime
-- level (all describe the same thing).
Prelude> data UnaryTypeCon a = UnaryValueCon a
--            0                1
-- 0: UnaryTypeCon is a type constructor with one argument. Awaiting a type
-- constant to be applied to. Type-level functions exist, but aren't covered
-- here.
--
-- 1: UnaryValueCon is a data constructor with one value. Careful that
-- not all type arguments to constructors have value-level witnesses.
-- (PERSONAL NOTE: Not sure what phantom witnesses are in this case...)
Prelude>
```
