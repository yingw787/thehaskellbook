# Chapter 29

- I/O
    - Keeps pure functions from effectful ones
    - Effects: Observable impact on evaluation environment
        - stdout / stdin / destroying state
        - We lose guarantee of idempotency when IO is introduced

- Where I/O explanations go astray
    - IO is not a Monad
        - IO is a type, Monad is a typeclass instance

- Burn the `State` to the ground
    - GHC docs even say: "The IO Monad is just an instance of the ST Monad,
      where the state is the real world".
    - However, in IO, you don't see / interact underlying State
        - For IO, State just tells GHC what order IO actions are in, and what a
          unique IO action is
    - `RealWorld#` description in `GHC.Prim` is an interesting read

- The reason we need this type
    - A way to order operations
    - A way to disable sharing that takes place in non-strictness

- Order and chaos

```haskell
-- IO can guarantee that this will print "123", because it nests actions and
-- provides sequencing and ordering.
main = do
    putStr "1"
    putStr "2"
    putStrLn "3"
```

- Sharing
    - Usually, we can assume evaluating a function will return a value
    - With IO, you're not guaranteed anything
        - `IO a` is a description for how you *might* `a`.

- The time has come

```haskell
-- from Data.Time.Clock
--
-- Without IO, getCurrentTime would share the time from the first time it was
-- forced
--
-- Instead, IO describes how you could get the current time when you need it
getCurrentTime :: IO UtcTime
```

- Another example
    - `criterion` and `whnf` / `nf` had to be applied to arguments to disable
      sharing; otherwise benchmarks would only tell us the performance of one
      run instead of multiple runs

    - However, `whnfIO` and `nfIO` don't have this problem; sharing is prevented
      through `IO` action.

- The code! It doesn't work!
    - See "Parallel & Concurrent Programming in Haskell", by Simon Marlow

    - See `WhatHappens.hs`.

- IO doesn't disable sharing for everything
    - See `TraceSharing.hs`.

- Purity is losing meaning
    - purity not always equal to "without effects"

- Semantically, pedantically accurate
    - Originally, "purely functional" -> lambda calculus semantics for the
      entire language

- Referential transparency
    - Purity as referential transparency vs. purity as a lambda calculus is that
      lambda calculus is a stronger guarantee than referential transparency
    - IO is still referentially transparent, because it will *generate the same
      IO action every time*
        - (PERSONAL NOTE: Hmm, haven't thought about IO in that regards; I
          thought effects impacted whether it was referentially transparent, but
          I guess that is true, if you have the same environment and the same IO
          actions, you get the same effect)

    - See `IORefTrans.hs`.

- IO's Functor, Applicative, and Monad
