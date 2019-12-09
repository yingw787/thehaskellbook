# Chapter 8

- Recursion: self-referential expressions

- Important because it allows us to define something indefinite, or incremental
  computation without having to duplicate source code.

- On the surface, lambda calculus doesn't seem to have a notion for recursion.
    - Being able to write recursive functions is essential to Turing completeness
        - Turing completeness: Can simulate a Turing machine (have some data
          representation, and some operations like reading, editing (writing or
          nullifying), and moving the tape)
    - Y combinator, or fixed point combinator, helps write recursive functions
      in the lambda syntax.

- Factorial

```haskell
Prelude> :{
Prelude| fourFactorial :: Integer
-- Only handles one exact case of factorial. impractical.
Prelude| fourFactorial = 4 * 3 * 2 * 1
Prelude| :}
Prelude> :{
Prelude| brokenFact1 :: Integer -> Integer
-- Broken, never stops because there's never a base case.
Prelude| brokenFact1 n = n * brokenFact1 (n - 1)
Prelude| :}
Prelude> :{
Prelude| factorial :: Integer -> Integer
-- Success, because now we have a base case at 0.
Prelude| factorial 0 = 1
Prelude| factorial n = n * factorial (n - 1)
Prelude| :}
Prelude> factorial 4
24
Prelude>
```
