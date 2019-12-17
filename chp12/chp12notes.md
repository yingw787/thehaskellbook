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
