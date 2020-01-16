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
