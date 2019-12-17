# Chapter 12

- Signaling adversity
    - Explicit datatypes signal when function received combination of inputs
      that don't make sense.

- How to stop worrying and love `Nothing`
    - When we don't have a value, return `Nothing` by default

```haskell
-- (What to put here?)
-- ifEvenAdd2 :: Integer -> Integer
-- ifEvenAdd2 n = if even n then n + 2 else ???

-- (This doesn't work since `n + 2` is not of the type `Maybe Integer`)
-- ifEvenAdd2 :: Integer -> Maybe Integer
-- ifEvenAdd2 n = if even n then n + 2 else Nothing

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n + 2) else Nothing
```

- Smart constructors for datatypes

```haskell
-- Define a Person, with attributes Name and Age.
type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

-- Handle cases of invalid inputs.
--
-- Called a smart constructor (construct values only when they meet certain
-- criteria, and an explicit signal when we do not)
mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
    | name /= "" && age >= 0 = Just $ Person name age
    | otherwise = Nothing
```

- Bleating `Either`

```haskell
-- `Either` handles cases where an exception might otherwise occur.
data Either a b = Left a | Right b

-- Important to derive `Eq` to equality check constructors.
data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)
```

See `EqCaseGuard.hs`.

- Kinds, a thousand stars in your types
    - Describe types of type constructors.
    - Type constants: types that take no arguments and are already types.
    - Type constructor: types that must have arguments applied to become a type.

```haskell
-- `Example` is a type constructor because it takes in a type argument `a`.
-- `* -> *`
data Example a = Blah | Woot a
```

- `Maybe` is a type constructor with one type argument.
- `Either` is a type constructor with two type arguments.

- Lifted and unlifted types
    - Lifted type: any that can be inhabited by `bottom`.
        - Represented by a pointer
    - Unlifted type: cannot be represented by `bottom`.
        - Native machine types and raw pointers.
    - `newtype` are special: they are kind `*` but are unlifted, which means
      they cannot create any new pointer, which means it cannot be inhabited by
      `bottom` (only contained type can be inhabited by `bottom`).

- You could wrap `Maybe` within a `Maybe` if it is fully applied and the kind is
  `*`. Same goes for other higher-order types that are fully applied and satisfy
  typeclass constraints.

- Data constructors are functions
    - They are curried, as well, just like Haskell functions.

```haskell
-- Nullary data constructors are not like functions:
-- `Trivial 1` raises a runtime exception.
data Trivial = Trivial deriving Show

-- Unary data constructors are like functions
-- They can be typechecked
data UnaryC = UnaryC Int deriving Show
-- They can be parametrized with a type variable instead of hardcoded type
data UnaryC a = UnaryC a deriving Show

-- You can apply a type constructor to values as well:
-- fmap Just [1, 2, 3] = [Just 1, Just 2, Just 3]
```

********** BEGIN CHAPTER EXERCISES **********

Determine the kinds:

1. `a` has kind `*`. (CORRECT BY ANSWER KEY
   https://github.com/johnchandlerburnham/hpfp)

2. `a` has kind `*`, while `f` has kind `* -> *`. (CORRECT BY ANSWER KEY
   https://github.com/johnchandlerburnham/hpfp)

String processing

See `StringProcessing.hs`.

Validate the word

See `ValidateTheWord.hs`.

It's only natural

See `ItsOnlyNatural.hs`.

Small library for Maybe

See `SmallLibraryForMaybe.hs`.

Small library for Either

See `Small LibraryForEither.hs`.

Write your own iterate and unfoldr

See `IterateAndUnfoldr.hs`.

Finally something other than a list!

See `OtherThanList.hs`.

********** END CHAPTER EXERCISES **********
