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
