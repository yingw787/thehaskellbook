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

```haskell
Prelude> print "hello world!" -- literal print (incl. quotes) and newline
"hello world!"
Prelude> putStrLn "hello world!" -- print without quotes
hello world!
Prelude> putStr "hello world!" -- print without newline
hello world!Prelude>
```
