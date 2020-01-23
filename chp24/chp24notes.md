# Chapter 24

- Parser combinators
    - Parsing: accept serialized input (characters or bytes), and turn it into a
      syntax representation (possibly tree-like)

- A few more words of introduction
    - This chapter covers only what you need; may not be long enough for making
      your own parser (like I want to make a parser).

- Understanding the parsing process
    - Parser: function that takes text input and outputs "structure" as output
    - Parser combinator: higher-order function that takes parser as input and
      returns a new parser as output

    - Oftentimes parser combinators use `Reader`-like implicit argument passing

    - Combinators enable recursion and composition / modularity

- Since we didn't use an analogy for `Monad`
    - See `LearnParsers.hs`.

- A bit like...
    - `State` + failure

```haskell
type Parser a = String -> Maybe (a, String)
-- Await a string value
-- Generate a 'Nothing' value upon failure
-- Returns parsed output plus remainder of String

newtype Reader r a = Reader { runReader :: r -> a }

newtype State s a = State { runState :: s -> (a, s) }
```

```haskell
Prelude> import Control.Monad.Trans.State
Prelude Control.Monad.Trans.State> runStateT (put 8) 7
((),8)
Prelude Control.Monad.Trans.State> runStateT get 8
(8,8)
Prelude Control.Monad.Trans.State> runStateT (put 1 >> get) 8
(1,1)
Prelude Control.Monad.Trans.State> (runStateT $ put 1 >> get) 0
(1,1)
Prelude Control.Monad.Trans.State> (runStateT $ put 2 >> get) 10002029302149
(2,2)
Prelude Control.Monad.Trans.State> (runStateT $ put 2 >> return 9001) 0
(9001,2)
Prelude Control.Monad.Trans.State>
```

- Back to our regularly scheduled coding
    - See `LearnParsers.hs`.

********** BEGIN EXERCISES: PARSING PRACTICE **********

See `ParsingPractice.hs`.

********** END EXERCISES: PARSING PRACTICE **********

- Intermission: parsing free jazz
    - Not sure how method 'parseByteString' is meant to be parsed in GHC v8.4.3

- Parsing fractions
    - See `Fractions.hs`.

********** BEGIN EXERCISE: UNIT OF SUCCESS **********

```haskell
-- FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp
unitSuccess :: String -> Parser String
unitSuccess str = do
    a <- string str
    eof
    return a
```

********** END EXERCISE: UNIT OF SUCCESS **********

- Haskell's parsing ecosystem
    - Check out `attoparsec` for performance, `cassava` for CSV parsing

- Type classes of parsers

- Alternative
    - See `AltParsing.hs`.

- QuasiQuotes
    - See `Quasimodo.hs`.

```haskell
Prelude> :set -ddump-splices
Prelude> :l Quasimodo.hs
[1 of 1] Compiling Quasimodo        ( Quasimodo.hs, interpreted )
Quasimodo.hs:(10,15)-(15,2): Splicing expression
    Language.Haskell.TH.Quote.quoteExp
      r
      "\n\
      \123\n\
      \abc\n\
      \456\n\
      \def\n"
  ======>
    "
123
abc
456
def
"
Ok, one module loaded.
*Quasimodo>
```

- Return to Alternative
    - See `AltParsing.hs`.

********** BEGIN EXERCISE: TRY TRY **********

```haskell
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
parseDecimal :: Parser Rational
parseDecimal = do
    while <- decimal
    char '.'
    part <- decimal
    case part of
        0 -> return (toRational whole)
        _ -> return (makeDecimal whole part)

-- Additional answers elided
```

********** END EXERCISE: TRY TRY **********

- Parsing configuration files
    - See `Ini.hs`.
