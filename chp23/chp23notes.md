# Chapter 23

- `State`
    - State: data that exists apart from input and output.

- What is state?
    - Think of ripple carry adders or light switches
    - Haskell doesn't allow state mutation; only arguments and return values
        - Use `State` monad to interface with state
        - Use `ST` monad transformer (ST) to mutate state in place

    - In Haskell, state can exist separate from I/O, remains highly scoped,
      maintains referential transparency, and is explicit in types in functions
        - (PERSONAL NOTE: Huh, I thought Haskell state would be more similar to
          something like Redux, a functional state store)
    - There are other ways to share state in Haskell, `State` is used when you
      have no other operational constraints.

- Random numbers

```haskell
Prelude> import System.Random
Prelude System.Random> mkStdGen 0
1 1
Prelude System.Random> :t mkStdGen 0
mkStdGen 0 :: StdGen
Prelude System.Random> sg = mkStdGen 0
Prelude System.Random> :t next sg
next sg :: (Int, StdGen)
Prelude System.Random> next sg
(2147482884,40014 40692)
Prelude System.Random> next sg
(2147482884,40014 40692)
Prelude System.Random> snd (next sg)
40014 40692
Prelude System.Random> newSg = snd (next sg)
Prelude System.Random> :t newSg
newSg :: StdGen
Prelude System.Random> next newSg
(2092764894,1601120196 1655838864)
Prelude System.Random> next (snd (next newSg))
(1390461064,1346387765 2103410263)
Prelude System.Random> :t random newSg
random newSg :: Random a => (a, StdGen)
Prelude System.Random> random newSg :: (Int, StdGen)
(138890298504988632,439883729 1872071452)
Prelude System.Random> random newSg :: (Double, StdGen)
(0.41992072972993366,439883729 1872071452)
```

- The `State` newtype

```haskell
newtype State s a = State { runState :: s -> (a, s) }

-- Similar to Reader newtype
newtype Reader r a = Reader { runReader :: r -> a }
```

- Throw down
    - See `RandomExample.hs`.
        - This produces a random but deterministic sequence of die rolls.
    - See `RandomExample2.hs`.

********** BEGIN EXERCISE: ROLL YOUR OWN **********

See `RollYourOwn.hs`.

********** END EXERCISE: ROLL YOUR OWN **********

- Write `State` for yourself
    - `WriteStateForYourself.hs`.

- Get a coding job with one weird trick
    - Get `FizzBuzz.hs`.
