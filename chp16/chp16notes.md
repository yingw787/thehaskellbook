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
