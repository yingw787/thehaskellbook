# Chapter 20

- `Foldable`
    - Lists are foldable data structures
    - Foldability and catamorphisms are generalizable to many datatypes

- List fold: Reduce values inside list to one summary value, by recursively
  applying some function.
    - filter, map are folding operations that return a new list as summary value

- Folding is always dependent on instance with `Monoid` typeclass instance
