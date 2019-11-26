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
