# Chapter 16

- Functors
    - Monoids taught us how to take an algebra and turn it into a typeclass.
    - Functors, Applicatives, and Monads will be similar.

    - Functor: Pattern of mapping over structure
    - `fmap`: Map, but over more data structures than just lists

- What's a functor?
    - Way to apply a function around some structure we don't want to alter
    - Apply to values within structure w/o affecting the structure itself
    - In Haskell, this is expressed usinga typeclass.

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
--           1          2      3
-- 1: '(a -> b)' represents a function input argument
-- 2: 'f a' is a Functor f that takes type argument a.
-- 3: 'f b' is the return value. Same f as from 'f a', type argument 'b'
-- may but not necessarily refers to a different type (type can float from 'a').
```

- There's a whole lot of `fmap` goin' round

```haskell
-- 'fmap' has superset of operations of 'map' w.r.t. lists.
Prelude> map (\x -> x > 3) [1..6]
[False,False,False,True,True,True]
Prelude> fmap (\x -> x > 3) [1..6]
[False,False,False,True,True,True]
-- 'map' doesn't work around the 'Just' type, while 'fmap' does.
Prelude> map (+1) (Just 1)

<interactive>:3:11: error:
    • Couldn't match expected type ‘[b]’
                  with actual type ‘Maybe Integer’
    • In the second argument of ‘map’, namely ‘(Just 1)’
      In the expression: map (+ 1) (Just 1)
      In an equation for ‘it’: it = map (+ 1) (Just 1)
    • Relevant bindings include it :: [b] (bound at <interactive>:3:1)
Prelude> fmap (+1) (Just 1)
Just 2
-- 'fmap' works with tuples
--
-- (PERSONAL NOTE: This apparently works only with 2-tuples; changing the number
-- of values of the tuple appears to break in GHCi. I don't think this method is
-- mapping the function over all values of the tuple.)
Prelude> fmap (10/) (4, 5)
(4,2.0)
Prelude> rca = Right "Chris Allen"
Prelude> fmap (++ ", Esq.") rca
Right "Chris Allen, Esq."
Prelude>
```
