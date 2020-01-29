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
