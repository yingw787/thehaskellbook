# Chapter 22

- `Reader`
    - How do you handle globals (data needed by the entire application)?
    - Creating arguments just for globals would bloat the codebase
    - We can use `Reader`.

- A new beginning
    - See `ANewBeginning.hs`.
    - You can have a functor, applicative, and monad for partially applied
      functions.
        - Functor of functions is function composition.
        - Applicative and Monad chain argument forward in addition to
          composition.

- Reader strings functions together when all those functions await one input
  from shared env

********** BEGIN EXERCISE: WARMING UP **********

See `WarmingUp.hs`.

********** END EXERCISE: WARMING UP **********

- This is `Reader`
    - Usually refers to monadic contexts of functions, as opposed to functorial
      or applicative contexts
    - `Reader`: Reading an argument from environment into functions.

- Breaking down the `Functor` of functions

- But uh, `Reader`?

```haskell
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader $ \r -> f (ra r)

-- Same as (.)
compose :: (b -> c) -> (a -> b) -> (a -> c)
compose f g = \x -> f (g x)

-- Slightly different instance for `Reader`
instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader $ (f . ra)
```

********** BEGIN EXERCISE: ASK **********

See `Ask.hs`.

********** END EXERCISE: ASK **********
