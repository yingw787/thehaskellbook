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

********** EXERCISES: COMPREHENSION CHECK **********

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

2.  Write one function that has one parameter and works for all the following
    expressions. Be sure to name the function.

    3.14 * (5 * 5)
    3.14 * (10 * 10)
    3.14 * (2 * 2)
    3.14 * (4 * 4)

    __________

    ```haskell
    Prelude> piSelf x = 3.14 * (x * x)
    Prelude> piSelf 5
    78.5
    Prelude> 3.14 * (5 * 5)
    78.5
    Prelude> piSelf 10
    314.0
    Prelude> 3.14 * (10 * 10)
    314.0
    Prelude> piSelf 2
    12.56
    Prelude> 3.14 * (2 * 2)
    12.56
    Prelude> piSelf 4
    50.24
    Prelude> 3.14 * (4 * 4)
    50.24
    Prelude>
    ```

3.  There is a value in `Prelude` called `pi`. Rewrite your function to use `pi`
    instead of 3.14.

    __________

    ```haskell
    Prelude> piSelf x = pi * (x * x)
    Prelude> piSelf 5
    78.53981633974483
    Prelude> pi * (5 * 5)
    78.53981633974483
    Prelude> piSelf 10
    314.1592653589793
    Prelude> pi * (10 * 10)
    314.1592653589793
    Prelude> piSelf 2
    12.566370614359172
    Prelude> pi * (2 * 2)
    12.566370614359172
    Prelude> piSelf 4
    50.26548245743669
    Prelude> pi * (4 * 4)
    50.26548245743669
    Prelude>
    ```

********** END EXERCISES: COMPREHENSION CHECK **********

Operators vs. Functions: some operators are infix (applied between two
arguments), but Haskell functions default to prefix syntax.
-   Sometimes, you can use functions infix-style: `10 ``div`` 4` vs. `div 10 4`.
-   Sometimes, you can use infix operators in prefix fashion: `(+) 100 100` vs.
    `100 + 100`.

Default associativity / precedence to infix operators (*), (+), (-), (/):
[PEDMAS](https://en.wikipedia.org/wiki/PEMDAS).

`infixl` in documentation means it's an infix operator that is left associative.
(*) is left associative (2 * 3 * 4 -> (2 * 3) * 4)).
`infixr` in documentation means it's an infix operator that is right
associative. (^) is right associative (2 ^ 3 ^ 4 -> 2 ^ (3 ^ 4))

********** EXERCISES: PARENTHESES AND ASSOCIATION **********

********** END EXERCISES: PARENTHESES AND ASSOCIATION **********
