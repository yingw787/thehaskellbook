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

- Type constructors and kinds
    - Kind signature: Type of types of types one level up.
    - Represented by `*`.
    - Something is fully applied and concrete when represented as a kind (`*`).
    - Something is waiting to be applied when it is a function (`* -> *`).

```haskell
Prelude> f = not True
Prelude> :t f
-- Of the form `*`.
f :: Bool
Prelude> f x = x > 3
Prelude> :t f
-- Of the form `* -> *`.
f :: (Ord a, Num a) => a -> Bool
-- We can query the kind signature of a type constructor using GHCi method
-- `:kind`.
Prelude> :kind Bool
Bool :: *
Prelude> :kind [Int]
[Int] :: *
Prelude> :kind []
[] :: * -> *
Prelude>
```

- Data constructors and values

```haskell
-- PugType is a type constant, that enumerates one constructor.
--
-- PugData is a constant value data constructor. Any function that requires
-- a value of type PugType, that value will always be PugData.
Prelude> data PugType = PugData
-- HuskyType is a type constructor, enumerates one constructor, taking in a
-- single parametrically polymorphic type variable as an argument.
--
-- HuskyData is a data constructor with no arguments. Type argument `a` is
-- a phantom or "has no witness". HuskyData is a constant value.
Prelude> data HuskyType a = HuskyData
-- DogueDeBordeaux is a type constructor, enumerating one constructor, with
-- one variable argument (equivalent to `a` through alpha equivalence).
--
-- The data constructor has the same name as the type constructor, but they
-- are different things. As the variable in the data constructor appears in
-- the type constructor, they are the same data.
Prelude> data DogueDeBordeaux doge = DogueDeBordeaux doge
-- We can use these definitions in constructing the following values.
--
-- We can construct `myPug` with or without casting to PugType.
Prelude> myPug = PugData
Prelude> myPug = PugData :: PugType
-- We can apply whatever concrete types or constrained / free polymorphic
-- types when casting myHusky or myOtherHusky, because the type variable
-- is a phantom variable that does not affect the data constructor.
Prelude> myHusky :: HuskyType a; myHusky = HuskyData
Prelude> myOtherHusky :: Num a => HuskyType a; myOtherHusky = HuskyData
-- We can apply a type and a matching value when constructing values of
-- type DogueDeBordeaux.
Prelude> myDoge :: DogueDeBordeaux Int; myDoge = DogueDeBordeaux 10
-- If the type and value for the type variable of DogueDeBordeaux do not
-- match, then a compile-time error occurs because the value cannot be
-- reconciled.
Prelude> badDoge :: DogueDeBordeaux String; badDoge = DogueDeBordeaux 10

<interactive>:18:62: error:
    • No instance for (Num String) arising from the literal ‘10’
    • In the first argument of ‘DogueDeBordeaux’, namely ‘10’
      In the expression: DogueDeBordeaux 10
      In an equation for ‘badDoge’: badDoge = DogueDeBordeaux 10
Prelude>
```

```haskell
-- Type signature for Doggies looks very much like a function.
Prelude> data Doggies a = Husky a | Mastiff a deriving (Eq, Show)
-- Needs to be applied to become a concrete type.
Prelude> :k Doggies
Doggies :: * -> *
-- Needs to be applied to become a concrete value.
Prelude> :t Husky
Husky :: a -> Doggies a
Prelude>
```
