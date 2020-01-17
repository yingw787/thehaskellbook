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
    - Maps function over structure (like `fmap`), but also generates more
      structure (like `(=<<)`).

```haskell
traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
traverse f = sequenceA . fmap f

-- looks somewhat like `fmap` and `(=<<)` (flip bind)
fmap :: Functor f => (a -> b) -> f a -> f b

(=<<) :: Monad m => (a -> m b) -> m a -> m b
```

```haskell
Prelude> fmap Just [1, 2, 3]
[Just 1,Just 2,Just 3]
Prelude> sequenceA $ fmap Just [1, 2, 3]
Just [1,2,3]
Prelude> sequenceA . fmap Just $ [1, 2, 3]
Just [1,2,3]
Prelude> traverse Just [1, 2, 3]
Just [1,2,3]
```

- So, what's `Traversable` for?
    - Anytime you flip to type constructors around, or map something and then
      flip them around

```haskell
Prelude> f = undefined :: a -> Maybe b
Prelude> xs = undefined :: [a]
Prelude> :t map f xs
map f xs :: [Maybe b]
Prelude> :t sequenceA $ map f xs
sequenceA $ map f xs :: Maybe [a]
Prelude> :t traverse f xs
traverse f xs :: Maybe [b]
```

- Morse code revisited

```haskell
-- What we did
stringToMorse :: String -> Maybe [Morse]
stringToMorse s = sequence $ fmap charToMorse s

-- What we want
stringToMorse :: String -> Maybe [Morse]
stringToMorse = traverse charToMorse
```

- Axing tedious code

```haskell
-- Original code
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
    a <- fetchFn query
    case sequence (map decodeFn a) of
        (Left err) -> return $ Left err
        (Right res) -> do
            a <- makeIoOnlyObj res
            return $ Right a

-- Refactored pipelineFn
pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
    traverse makeIoOnlyObj (mapM decodeFn a)

-- Refactored pipelineFn
pipelineFn'' :: Query -> IO (Either Err (SomeObj, IoOnlyObj))
pipelineFn'' = ((traverse makeIoOnlyObj . mapM decodeFn) =<<) . fetchFn

-- Refactored again, preferred
pipelineFn2 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn2 = ((traverse makeIoOnlyObj . traverse decodeFn) =<<) . fetchFn
```

- Do all the things

```haskell
module HttpStuff where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq

urls :: [String]
urls =  [ "http://httpbin.org/ip"
        , "http://httpbin.org/bytes/5"
        ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls
```

- Traversable instances
