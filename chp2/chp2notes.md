# Chapter 2

- Relevant links to get started:
    - https://github.com/bitemyapp/learnhaskell
    - haskellstack.org
    - `curl https://get-ghcup.haskell.org -sSf | sh`

- Use `stack ghci` to apply versioning, with a YAML file `stack.yaml` for
  versioning.

- Hello World

ghc(i) usage notes:
    - :quit / :q to quit
    - :info / :i for information
    - :load to load a Haskell file
    - :reload to refresh dev environment.

Haskell = (expression | declaration)
    - expressions: values, combinations of values, functions applied to values, evaluate to a result.
    - declarations: top-level binds to name expressions.

Normal form expressions: irreductible form.

Function: Haskell expression that is applied to an argument and returns a result.
    - All functions in Haskell take one argument and return one result; multiple
    arguments into a function is currying underneath.

Function definitions:
    - Argument vs. parameter: Argument referes to function's parameters when
    applied, and not the variables that represent the function definition.
