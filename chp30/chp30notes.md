# Chapter 30

- When things go wrong

- Exceptions
    - Exception: Condition that has interrupted expected program execution

    - Raising exceptional conditions via datatypes isn't always ideal
        - Exceptions can be faster than passing a `Nothing` through a full
          pipeline

    - Allowing exceptions to halt program anytime is suboptimal
        - (PERSONAL NOTE: Interesting philosophy; both Python and Erlang,
          especially Erlang, espouses "let it crash" and crash fast philosophy)

- The `Exception` class and methods

```haskell
class (Typeable e, Show e) => Exception e where
    toException :: e -> SomeException
    fromException :: SomeException -> Maybe e
    displayException :: e -> String
```

- There are actually quite a few exception types available
    - See `Control.Exception`

- A brief introduction to existential quantification
    - `SomeException` is a parent type for all other exception types
        - (PERSONAL NOTE: I'm guessing something like `BaseException`)

- GADT (generalized algebraic datatype)
    - enclose existential quantification
    - Moving quantifier to data constructor limits application scope
        - `for all e` to `there exists some e`.
        - Any type that implements `Exception` can be `e`, and can be subsumed
          under `SomeException`.

```haskell
-- GADT
data SomeException where
    SomeException :: Exception e => e -> SomeException

-- Rewritten, same as above
data SomeException =
    forall e . Exception e => SomeException e
```

- So, wait, what?
    - See `WhySomeException.hs`

- Data.Typeable
    - Typeclass permits types to be known at runtime, like dynamic typechecking
    - Useful for when you need to have various types to be passed but needs to
      enforce or trigger on specific types
        - (PERSONAL NOTE: I'm curious as to why you don't just have additional
          type signatures and break out the method)

    - Makes sense for exceptions, because you need to match against the correct
      Exception type, and you only know the correct type at runtime
        - (PERSONAL NOTE: I guess)

- This machine kills programs
    - Running code is an I/O operation, so exceptions (runtime events) usually
      happen during I/O.
    - I/O's implicit contract is "you cannot expect this computation to succeed
      unconditionally"
    - Anything can fail, even `putStrLn`; depending on UNIX file permissions

- Want either? Try!
    - You may want to lift an exception into an explicit `Either` value

```haskell
try :: Exception e => IO a -> IO (either e a)
```

```haskell
module TryExcept where

import Control.Exception


willIFail :: Integer -> IO (Either ArithException ())
willIFail denom = try $ print $ div 5 denom
```

- Exceptions are imprecise
    - They will reverberate up the stack and kill the program or get caught

- The unbearable imprecision of trying

```haskell
import Control.Exception

-- `throwIO` raises a number of different types of exceptions
canICatch :: Exception e => e -> IO (Either ArithException ())
canICatch e = try $ throwIO e
```

- See `StoppingTheParty.hs`.

- Why `throwIO`?
    - Sometimes, we do want to stop a program when a certain condition occurs

```haskell
import Control.Exception


main :: IO ()
main = do
    throwIO DivideByZero
    putStrLn "lol"
```

- Making our own exception types
    - See `OurExceptions.hs`.

- Surprising interaction with `bottom`
    - Due to non-strictness, your bottom could've been forced before or after
      your exception handler

```haskell
import Control.Exception


-- These give two different results
noWhammies :: IO (Either SomeException ())
noWhammies = try undefined

megaButtums :: IO (Either SomeException ())
megaButtums = try $ return undefined
```

- Asynchronous Exceptions
    - Most languages don't have anything like this because they can't hope to
      make it safe within their implementation runtimes
        - (PERSONAL NOTE: This is really interesting...need to come back to
          this; yea what happens when Python 3.x async runs into an exception?)

    - See `AsyncExceptions.hs`.
