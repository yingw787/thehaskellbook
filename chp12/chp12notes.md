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
