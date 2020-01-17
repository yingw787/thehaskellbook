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
    - `Data.Maybe.catMaybe` has a different may of handling `Maybe` values
        - Can allow you to sum list of `Maybe` values even if there are
          `Nothing` values

```haskell
Prelude> import Data.Maybe
Prelude Data.Maybe> :t catMaybes
catMaybes :: [Maybe a] -> [a]
Prelude Data.Maybe> :t sequenceA
sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
Prelude Data.Maybe> xs = [Just 1, Just 2, Just 3]
Prelude Data.Maybe> catMaybes xs
[1,2,3]
Prelude Data.Maybe>  sequenceA xs
Just [1,2,3]
Prelude Data.Maybe> xsn = [Just 1, Just 2, Nothing]
Prelude Data.Maybe> catMaybes xsn
[1,2]
Prelude Data.Maybe> sequenceA xsn
Nothing
Prelude Data.Maybe> xsn' = xs ++ [Nothing]
Prelude Data.Maybe> sum $ catMaybes xsn'
6
Prelude Data.Maybe> fmap sum $ sequenceA xsn'
Nothing
```

- `traverse`
