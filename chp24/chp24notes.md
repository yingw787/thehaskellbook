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
