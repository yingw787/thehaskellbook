# Chapter 3

- Strings

- :: -> "has the type" (type signature)

```haskell
Prelude> :type 'a'
'a' :: Char
Prelude>
```

- single quotes are for characters, double quotes are for strings (char
  sequence, or list of char).

```haskell
Prelude> :type "Hello!"
"Hello!" :: [Char]
Prelude>
```

Print in REPL:

```haskell
Prelude> print "hello world!" -- literal print (incl. quotes) and newline
"hello world!"
Prelude> putStrLn "hello world!" -- print without quotes
hello world!
Prelude> putStr "hello world!" -- print without newline
hello world!Prelude>
```

Print from file:

```haskell
Prelude> :l print1.hs
[1 of 1] Compiling Print1           ( print1.hs, interpreted )
Ok, one module loaded.
*Print1> main
hello world!
*Print1>
```

Change default GHCi prompt:

```haskell
Prelude> :set prompt "λ> "
λ>
```

GHCi by default implements `main :: IO ()` to print to the prompt.

Execute sequence of operations using the `do` operator.

```haskell
Prelude> :l print2.hs
[1 of 1] Compiling Print2           ( print2.hs, interpreted )
Ok, one module loaded.
*Print2> main
Count to four for me:
one, two, three, and four!
*Print2>
```

String concatenation

```haskell
Prelude> :l print3.hs
[1 of 1] Compiling Print3           ( print3.hs, interpreted )
Ok, one module loaded.
*Print3> main
hello world!
hello world!
*Print3>
```

top-level definition = module-scoped
local definition = expression-scoped

```haskell
Prelude> :l TopOrLocal.hs
[1 of 1] Compiling TopOrLocal       ( TopOrLocal.hs, interpreted )
Ok, one module loaded.
*TopOrLocal> topLevelFunction 5
20
*TopOrLocal> topLevelValue
5
*TopOrLocal> woot

<interactive>:4:1: error: Variable not in scope: woot
*TopOrLocal>
```

********** START EXERCISES: SCOPE **********

1. These lines of code are from a REPL session. Is y in scope for z?

```haskell
Prelude> x = 5
Prelude> y = 7
Prelude> z = x * y
```

(YES, y has been defined before z expression result is instantiated.)

(CORRECT):

```haskell
Prelude> x = 5
Prelude> y = 7
Prelude> z = x * y
Prelude> x
5
Prelude> y
7
Prelude> z
35
Prelude>
```

2. These lines of code are from a REPL session. Is h in scope for g? Go with
   your gut here.

```haskell
Prelude> f = 3
Prelude> g = 6 * f + h
```

(NO, if h has not been previously defined, then it will not be in the REPL
context.)

(CORRECT):

```haskell
Prelude> f = 3
Prelude> g = 6 * f + h

<interactive>:8:13: error: Variable not in scope: h
Prelude>
```

3.  This code sample is from a source file. Is everything we need to execute
    area in scope?

```haskell
area d = pi * (r * r)
r = d / 2
```

(YES, d / 2 can be substituted for r in area expression, and ordering does not
matter within a source file.)

(INCORRECT):

```haskell
Prelude> :l test.hs
[1 of 1] Compiling Main             ( test.hs, interpreted )

test.hs:4:5: error: Variable not in scope: d
  |
4 | r = d / 2
  |     ^
Failed, no modules loaded.
Prelude>
```

4. This code is also from a source file. Now are r and d in scope for area?

```haskell
area d = pi * (r * r)
    where r = d / 2
```

(YES, because r is a local variable using the where clause.)

(CORRECT):

```haskell
Prelude> :l test.hs
[1 of 1] Compiling Main             ( test.hs, interpreted )
Ok, one module loaded.
*Main> area 5
19.634954084936208
*Main>
```

********** END EXERCISES: SCOPE **********

- (++) is an infix operator
- `concat` is a normal function

```haskell
Prelude> :t (++)
-- a denotes an arbitrary type, this method definition enforces the same type
-- for arguments and return value. [a] is syntactic sugar for list[a], as
-- String = [Char].
--
-- Hence, a is "polymorphic"; all a needs to be for concat to work is the same.
--
-- Otherwise, type translation errors will result.
(++) :: [a] -> [a] -> [a]
Prelude> :t concat
concat :: Foldable t => t [a] -> [a]
Prelude>
```

********** START EXERCISES: SYNTAX ERRORS **********

Read the syntax of the following functions and decide whether it will compile.
Test them in your REPL and try to fix the syntax errors where they occur.

1. ++ [1, 2, 3] [4, 5, 6]

WILL CONCATENATE

(INCORRECT, infix operator must be wrapped in parentheses in order for it to be
used as a normal function).

```haskell
Prelude> ++ [1, 2, 3] [4, 5, 6]

<interactive>:3:1: error: parse error on input ‘++’
Prelude> (++) [1, 2, 3] [4, 5, 6]
[1,2,3,4,5,6]
Prelude>
```

2. '<3' ++ ' Haskell'

WILL CONCATENATE

(INCORRECT, strings or char sequences must be denoted with double quotes, not
single quotes.)

```haskell
Prelude> '<3' ++ ' Haskell'

<interactive>:5:2: error: parse error on input ‘<’
Prelude> "<3" ++ " Haskell"
"<3 Haskell"
Prelude>
```

3. concat ["<3", " Haskell"]

WILL CONCATENATE

(CORRECT)

```haskell
Prelude> concat ["<3", " Haskell"]
"<3 Haskell"
Prelude>
```

********** END EXERCISES: SYNTAX ERRORS **********

- (++) is a right-associative operator, therefore 'print3flipped.hs' has two
  different definitions of `secondGreeting` based on whether (++) is infixed or
  prefixed.

- Scoping can cause breakage, especially when wrapping variables.

```haskell
Prelude> :l print3broken.hs
[1 of 1] Compiling Print3Broken     ( print3broken.hs, interpreted )

print3broken.hs:6:14: error:
    Variable not in scope: greeting :: String
  |
6 |     putStrLn greeting
  |              ^^^^^^^^
Failed, no modules loaded.
Prelude>
```

- `print3broken.hs` --> `print3fixed.hs`.

- (:) (cons operator, builds a list.)

```haskell
Prelude> 'c' : "hris"
"chris"
Prelude> 'P' : ""
"P"
Prelude>
```

```haskell
-- `head` returns first element.
Prelude> head "Papuchon"
'P'
-- `tail` does NOT return the last element but rather everything except the
-- head.
Prelude> tail "Papuchon"
"apuchon"
-- `take` is a left-associative prefixed function that takes n characters.
Prelude> take 1 "Papuchon"
"P"
-- Result is still of type [Char]!
Prelude> take 0 "Papuchon"
""
Prelude> take 6 "Papuchon"
"Papuch"
-- No failure if taking more characters than present.
Prelude> take 150 "Papuchon"
"Papuchon"
-- `drop` is a left-associative prefixed function that drops n characters.
Prelude> drop 4 "Papuchon"
"chon"
-- Dropping more characters than present results in valid empty result.
Prelude> drop 9001 "Papuchon"
""
-- (!!) is an infix operator that returns the character at a particular index.
--
-- Kind of like a bitmask and reduction.
--
-- Haskell is 0-indexed.
Prelude> "Papuchon" !! 0
'P'
Prelude> "Papuchon" !! 4
'c'
Prelude>
```

- Many of these methods are "unsafe", in that they throw exceptions when given
  an empty list as input. They need to be made safe in a later chapter.

********** START CHAPTER EXERCISES **********

Reading Syntax

1. For the following lines of code, read the syntax carefully and decide if they
   are written correctly. Test them in your REPL after you've decided to check
   your work. Correct as many as you can.

   a) `concat [[1, 2, 3], [4, 5, 6]]`
   b) `++ [1, 2, 3] [4, 5, 6]`
   c) `(++) "hello" " world"`
   d) `["hello" ++ " world"]`
   e) `4 !! "hello"`
   f) `(!!) "hello" 4`
   g) `take "4 lovely"`
   h) `take 3 "awesome"`

2. Next we have two sets: the first set is lines of code and the other is a set
   of results. Read the code and figure out which results came from which lines
   of code. Be sure to test them in the REPL.

   a) `concat [[1 * 6], [2 * 6], [3 * 6]]`
   b) `"rain" ++ drop 2 "elbow"`
   c) `10 * head [1, 2, 3]`
   d) `(take 3 "Julie") ++ (tail "yes")`
   e) ```haskell
      concat [tail [1, 2, 3],
              tail [4, 5, 6],
              tail [7, 8, 9]]
      ```

    Can you match each of the previous expressions to one of these results
    presented in a scrambled order?

    a) "Jules"
    b) [2, 3, 5, 6, 8, 9]
    c) "rainbow"
    d) [6, 12, 18]
    e) 10

Building Functions

1.  Given the list-manipulation functions mentioned in this chapter, write
    functions that take the following inputs and return the expected outputs.
    Do them directly in your REPL and use the `take` and `drop` functions you've
    already seen.

    Example

    ```haskell
    -- If you apply your function
    -- to this value:
    "Hello World"
    -- Your function should return:
    "ello World"
    ```

    The following would be a fine solution:

    ```haskell
    Prelude> drop 1 "Hello World"
    "ello World"
    ```

    Now write expressions to perform the following transformations, just with
    the functions you've seen in this chapter. You do not need to do anything
    clever here.

    ```haskell
    -- (a)
    -- Given
    "Curry is awesome"
    -- Return
    "Curry is awesome!"

    -- (b)
    -- Given
    "Curry is awesome!"
    -- Return
    "y"

    -- (c)
    -- Given
    "Curry is awesome!"
    -- Return
    "awesome!"
    ```

2.  Now take each of the above and rewrite it in a source file as a general
    function that could take different string inputs as arguments but retain
    the same behavior. Use a variable as the argument to your (named) functions.
    If you're unsure how to do this, refresh your memory by looking at the
    `waxOff` exercise from the previous chapter and the `TopOrLocal` module
    from this chapter.

3.  Write a function of type `String -> Char` which returns the third character
    in a `String`. Remember to give the function a name and apply it to a
    variable, not a specific `String`, so that it could be reused for different
    `String` inputs, as demonstrated (feel free to name the function something
    else. Be sure to fill in the type signature and fill in the function
    definition after the equals sign):

    ```haskell
    thirdletter ::
    thirdLetter x =

    -- If you apply your function
    -- to this value:
    "Curry is awesome"
    -- Your function should return
    'r'
    ```

    Note that programming languages conventionally start indexing things by
    zero, so getting the zeroth index of a string will get you the first
    letter. Accordingly, indexing with 3 will get you the fourth. Keep this in
    mind as you write this function.

4.  Now change that function so the string operated on is always the same and
    the variable represents the number of the letter you want to return (you
    can use "Curry is awesome!" as your string input or a different string if
    you prefer).

5.  Using the `take` and `drop` functions we looked at above, see if you can
    write a function called `rvrs` (an abbrevation of `reverse` used because
    there is a function called `reverse` alrady n Prelude, so if you call your
    function the same name, you'll get an error message). `rvrs` should take the
    string "Curry is awesome" and return the result "awesome is Curry". This may
    not be the most lovely Haskell code you will ever write, but it is quite
    possible using only what we've learned so far. First write it as a single
    function in a source file. This doesn't need to, and shouldn't, work for
    reversing the words of any sentence. You're expected only to slice and dice
    this particular string with `take` and `drop`.

6.  Let's see if we can expand that function into a module. Why would we want
    to? By expanding it into a module, we can add more functions later that
    interact with each other. We can also then export it to other modules if we
    want to and use this code in those other modules. There are different ways
    you could lay it out, but for the sake of convenience, we'll show you a
    sample layout so that you can fill in the blanks:

    ```haskell
    module Reverse where

    rvrs :: String -> String
    rvrs x =

    main :: IO ()
    main = print ()
    ```

    Into the parentheses after `print` you'll need to fill in your function name
    `rvrs` plus the argument you're applying `rvrs` to, in this case "Curry is
    awesome." That `rvrs` function plus its argument are now the argument to
    `print`. It's important to put them inside the parentheses so that that
    function gets applied and evaluated first, and then that result is printed.

    Of course, we have also mentioned that you can use the $ symbol to avoid
    using parentheses, too. Try modifying your main function to use that instead
    of the parentheses.

********** END CHAPTER EXERCISES **********
