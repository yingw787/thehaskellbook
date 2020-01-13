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
```
