# Chapter 21

- `Traversable`
    - `Functor`: transform values embedded in structure, with pure function
    - `Applicative`: transform values embedded in structure, with function
      embedded in structure
        - Each function application adds structure that is then applicatively
          combined.
    - `Foldable`: process values embedded in structure as if they were ordered

    - `Traversable` was introduced in same paper as `Applicative`
        - `Foldable` <- `Traversable` <- `Applicative` <- `Functor`

        - Transform elements inside structure (like `Functor`)
        - Produce applicative effects
        - Maybe lift multiple instances of applicative structure outside
          traversable structure

        - A way to traverse a data structure: maps function inside structure,
          collects applicative contexts

- The `Traversable` type class definition

```haskell
-- `fmap` and `traverse` are oftentimes mistaken for each other
fmap :: Functor f => (a -> b) -> f a -> f b

traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

myData :: [String]
myFunc :: String -> IO Record

wrong :: [IO Record]
wrong = fmap myFunc myData

right :: IO [Record]
right = traverse myFunc myData
```

```haskell
-- `sequenceA` evaluates each action in structure from left to right and
-- collects the results.
sequenceA :: Applicative f => t (f a) -> f (t a)
sequenceA = traverse id
```

- `sequenceA`

```haskell
Prelude> fmap Just [1, 2, 3]
[Just 1,Just 2,Just 3]
Prelude> sequenceA $ fmap Just [1, 2, 3]
Just [1,2,3]
Prelude> xs = [Just 1, Just 2, Just 3]
Prelude> sequenceA xs
Just [1,2,3]
Prelude> xsn = [Just 1, Just 2, Nothing]
Prelude> sequenceA xsn
Nothing
Prelude> fmap sum $ sequenceA xs
Just 6
Prelude> fmap product (sequenceA xsn)
Nothing
```

- List of `Maybe` values to a `Maybe` of a list of values