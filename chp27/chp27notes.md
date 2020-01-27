# Chapter 27

- Nonstrictness

- Laziness
    - By chapter's end, should be able to reason about reduction process of
      Haskell expressions and evaluate strictness / non-strictness

    - Haskell has non-strict (*not lazy*) evaluation
        - Expressions are reduced only when necessary

    - A thunk (placeholder in AST) is created for each expression when
      evaluation process begins
        - If it doesn't need to be evaluated, it gets GC'ed
        - It it does, it can be shared amongst expressions

- Observational Bottom Theory
    - Strictness is defined by ability to evaluate expressions that have bottom
      in them
    - Bottom is a convenient way of observing evaluation

- Standards and obligations
    - A truly lazy language memoizes all results of all functions it evaluates,
      resulting in unacceptably large amounts of memory usage.

    - Non-strict: Same behavior w.r.t bottom
        - You can have an expressions which results in a value, even if bottom
          or infinite data lurks within
    - Lazy: takes specific approach to how program executes

```haskell
fst (1, undefined)
-- 1
snd (undefined, 2)
-- 2
```

- Outside in, inside out
    - Strict languages evaluate inside out
    - Non-strict languages evaluate outside in

```haskell
possiblyKaboom = \f -> f fst snd (0, undefined)

-- booleans as lambdas
true :: a -> a -> a
true = \a -> (\b -> a)

false :: a -> a -> a
false = \a -> (\b -> b)

possiblyKaboom' b =
    case b of
        True -> fst tup
        False -> snd tup
    where tup = (0, undefined)
```

- Outside-in evaluation is how you can touch the structure without touching the
  structure's contents (e.g. measuring size of list of bottoms)
