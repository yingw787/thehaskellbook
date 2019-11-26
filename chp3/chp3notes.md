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
