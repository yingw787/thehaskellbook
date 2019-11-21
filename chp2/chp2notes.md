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
    - :module / :m to return to `Prelude>` from any loaded Haskell package.

Haskell = (expression | declaration)
    - expressions: values, combinations of values, functions applied to values, evaluate to a result.
    - declarations: top-level binds to name expressions.

Normal form expressions: irreductible form.

Function: Haskell expression that is applied to an argument and returns a result.
    - All functions in Haskell take one argument and return one result; multiple
    arguments into a function is currying underneath.

Function definitions:
    - Argument vs. parameter: Argument refers to function's parameters when
    applied, and not the variables that represent the function definition.

Haskell doesn't evaluate everything to canonical or normal form; it evaluates to
[**weak head normal form
(WHNF)**](https://wiki.haskell.org/Weak_head_normal_form).
    - I cannot reproduce the example; I got this:

      ```haskell
        Prelude> (\f -> (1, 2 + f)) 2
        (1,4)
        Prelude>
      ```

      and not `(1, 2 + 2)` as expected

EXERCISES: COMPREHENSION CHECK


1.  Given the following lines of code as they might appear in a source file:

    ```haskell
    half x = x / 2

    square x = x * x
    ```

    Write the same declarations in your REPL and then use the functions half and
    square in some experimental expressions.

    __________

    ```haskell
    Prelude> half x = x / 2
    Prelude> half 5
    2.5
    Prelude> half 6
    3.0
    Prelude> half 8.0
    4.0
    Prelude> half "hi"

    <interactive>:5:1: error:
        • No instance for (Fractional [Char]) arising from a use of ‘half’
        • In the expression: half "hi"
        In an equation for ‘it’: it = half "hi"
    Prelude> square x = x * x
    Prelude> square 9
    81
    Prelude> square 3.141592658
    9.869604428799505
    Prelude> square 3
    9
    Prelude> square "hi"

    <interactive>:10:1: error:
        • No instance for (Num [Char]) arising from a use of ‘square’
        • In the expression: square "hi"
        In an equation for ‘it’: it = square "hi"
    Prelude>
    ```
