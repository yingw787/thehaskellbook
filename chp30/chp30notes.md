# Chapter 30

- When things go wrong

- Exceptions
    - Exception: Condition that has interrupted expected program execution

    - Raising exceptional conditions via datatypes isn't always ideal
        - Exceptions can be faster than passing a `Nothing` through a full
          pipeline

    - Allowing exceptions to halt program anytime is suboptimal
        - (PERSONAL NOTE: Interesting philosophy; both Python and Erlang,
          especially Erlang, espouses "let it crash" and crash fast philosophy)

- The `Exception` class and methods
