# Chapter 15

- Monoids and Semigroups

- What we mean by algebra
    - Study of mathematical symbols and usage governing their manipulation
    - Can be implemented with type classes
    - Set: type of operations are for
    - Instance: defines how each operation will perform for a given type or set

- Monoid
    - Binary associative operation with an identity
        - Monoid is the type class
        - Operation that is associative and has two arguments
        - A value within that type class that when combined with another value
          given an operation will always return that other value
    - Function that takes two arguments and follows two laws: associativity and
      identity
    - Typeclass that generalizes the above across type is the monoid

```haskell
Prelude> :i mappend
class Semigroup a => Monoid a where
...
mappend :: a -> a -> a
...
    -- Defined in ‘GHC.Base’
Prelude> mappend [1, 2, 3] [4, 5, 6]
[1,2,3,4,5,6]
Prelude> mappend [1..5] []
[1,2,3,4,5]
Prelude> mappend [] [1..5]
[1,2,3,4,5]
Prelude>
```
