# Chapter 19

- Applying structure

- Templating content in `scotty`
    - https://github.com/scotty-web/scotty

- Concatenating connection parameters
    - http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html

- Concatenating key configurations
    - Monoid of functions

```haskell
Prelude> import Data.Monoid
Prelude Data.Monoid> f = const (Sum 1)
Prelude Data.Monoid> g = const (Sum 2)
Prelude Data.Monoid> f 9001
Sum {getSum = 1}
Prelude Data.Monoid> g 9002
Sum {getSum = 2}
-- (PERSONAL NOTE: I didn't get the book definition of the Monoid of functions,
-- defined as `instance Monoid b => Monoid (a -> b)`.)
Prelude Data.Monoid> :t f
f :: Num a => b -> Sum a
Prelude Data.Monoid> :t g
g :: Num a => b -> Sum a
Prelude Data.Monoid> (f <> g) 9001
Sum {getSum = 3}
Prelude Data.Monoid>
```

```haskell
Prelude> import qualified Data.Map as M
Prelude M> :t M.fromList
M.fromList :: Ord k => [(k, a)] -> M.Map k a
Prelude M> let f = M.fromList [('a', 1)]
Prelude M> let g = M.fromList [('b', 2)]
Prelude M> :t f
f :: Num a => M.Map Char a
Prelude M> import Data.Monoid
Prelude M Data.Monoid> f <> g
fromList [('a',1),('b',2)]
Prelude M Data.Monoid> :t (f <> g)
(f <> g) :: Num a => M.Map Char a
Prelude M Data.Monoid> mappend f g
fromList [('a',1),('b',2)]
Prelude M Data.Monoid> f <> g
fromList [('a',1),('b',2)]
```

- `Functor`
    - Lifting over `IO`

```haskell
import Data.Time.Clock

offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset =
    fmap (addUTCTime (offset * 24 * 3600)) $
        getCurrentTime
```
