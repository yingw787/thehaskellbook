# Chapter 22

- `Reader`
    - How do you handle globals (data needed by the entire application)?
    - Creating arguments just for globals would bloat the codebase
    - We can use `Reader`.

- A new beginning
    - See `ANewBeginning.hs`.
    - You can have a functor, applicative, and monad for partially applied
      functions.
        - Functor of functions is function composition.
        - Applicative and Monad chain argument forward in addition to
          composition.

- Reader strings functions together when all those functions await one input
  from shared env

********** BEGIN EXERCISE: WARMING UP **********

See `WarmingUp.hs`.

********** END EXERCISE: WARMING UP **********
