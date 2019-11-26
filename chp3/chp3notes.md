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
