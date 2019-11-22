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

Below are some pairs of functions that are alike except for parenthesization.
Read them carefully and decide if the parentheses change the results of the
function. Check your work in GHCi.

1.  a)  8 + 7 * 9
    b)  (8 + 7) * 9
__________

a) and b) have different results, as multiplication has higher precedence than
addition.

```haskell
Prelude> 8 + 7 * 9
71
Prelude> (8 + 7) * 9
135
Prelude>
```

2.  a)  perimeter x y = (x * 2) + (y * 2)
    b)  perimeter x y = x * 2 + y * 2
__________

a) and b) have the same result, since multiplication has higher precedence than
addition.

3.  a)  f x = x / 2 + 9
    b)  f x = x / (2 + 9)
__________

a) and b) have different results, as division has higher precedence than
addition.

********** END EXERCISES: PARENTHESES AND ASSOCIATION **********

Order of declarations in source code doesn't matter because GHCI loads the
entire file at once (no import-time evaluation). Values into the REPL do depend
on ordering.

```haskell
Prelude> y = 10
Prelude> x = 10 * 5 + y
Prelude> myResult = x * 5
Prelude> myResult
300
Prelude> x
60
Prelude> y
10
Prelude>
```

Indentation matters.
Use spaces, not tabs, to indent your source code.

********** EXERCISES: HEAL THE SICK **********

The following code samples are broken and won't compile. The first two are as
you might enter into the REPL; the third is from a source file. Find the
mistakes and fix so that they will.

1. Error looks like:

```haskell
Prelude> area x = 3. 14 * (x * x)

<interactive>:1:1: error:
    • Non type-variable argument in the constraint: Num (b -> c)
      (Use FlexibleContexts to permit this)
    • When checking the inferred type
        area :: forall b c a.
                (Num (b -> c), Num (a -> c), Num (a -> b)) =>
                (a -> c) -> a -> c
Prelude>
```

Extra space between 3. and 14.

2. Error looks like:

```haskell
Prelude> double x = b * 2

<interactive>:3:12: error: Variable not in scope: b
Prelude>
```

Rename b to x.

3. For file `./heal_the_sick.hs` at commit
   `23976778f5aff0a4c48e6a88d899db7e49926a2d`, error looks like:

```haskell
Prelude> :load heal_the_sick.hs
[1 of 1] Compiling Main             ( heal_the_sick.hs, interpreted )

heal_the_sick.hs:2:4: error:
    parse error on input ‘=’
    Perhaps you need a 'let' in a 'do' block?
    e.g. 'let x = 5' instead of 'x = 5'
  |
2 |  y = 10
  |    ^
Failed, no modules loaded.
Prelude>
```

Remove extra indentation on line 2.

********** END EXERCISES: HEAL THE SICK **********

Three different ways of executing division: `(/)` (fractional division), `div`
(integral division, round down), and `quot` (integral division, round towards
zero).
- http://augustss.blogspot.com/
- https://stackoverflow.com/a/8111203

(negate) and `-` both negate values; `(negate 1234)` and `(-1234)` are the same.

The `($)` operator enables everything to the right of it to be evaluated first
and can be used to delay function application.

Expressions like `(*30)` are called sectioning:

```haskell
Prelude> 1 + 2
3
Prelude> (+) 1 2
3
Prelude> (+1) 2 -- sectioning
3
Prelude> (1+) 2 -- sectioning
3
```

Sectioning is useful if you want to apply functions to a data structure.

GHCi is good at loading one file at a time, but use Stack if you want to
maintain multi-module scope during runtime debugging.

********** EXERCISES: A HEAD CODE **********

Guess solution, then validate `let` expressions in the REPL.

1. 5 (CORRECT)
2. 25 (CORRECT)
3. 30 (CORRECT)
4. 3003 (INCORRECT, NO IDEA WHY I SAID THAT, actual answer is 6)

Rewrite with `where` clauses.

1. `first = x * 3 + y where x = 3; y = 1000`
2. `second = x * 5 where y = 10; x = 10 * 5 + y`
3. `third = z / x + y where x = 7; y = negate x; z = y * 10`

********** END EXERCISES: A HEAD CODE **********
