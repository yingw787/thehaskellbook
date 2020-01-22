# Chapter 23

- `State`
    - State: data that exists apart from input and output.

- What is state?
    - Think of ripple carry adders or light switches
    - Haskell doesn't allow state mutation; only arguments and return values
        - Use `State` monad to interface with state
