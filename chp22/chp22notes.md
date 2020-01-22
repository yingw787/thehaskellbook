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

- Functions have `Applicative` too

```haskell
-- r of Reader is the f structure in Functor instance.

-- Applicative f =>
-- f ~ (->) r
pure :: a -> f a
pure :: a -> (r -> a)

(<*>) :: f (a -> b) -> f a -> f b

(<*>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
```

- Demonstrating the function `Applicative`

```haskell
-- Differentiate between different "types"
newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

-- Two record types
data Person =
    Person {
        humanName :: HumanName
      , dogName :: DogName
      , address :: Address
    } deriving (Eq, Show)

data Dog =
    Dog {
        dogsName :: DogName
      , dogsAddress :: Address
    } deriving (Eq, Show)

-- Instances of record types
pers :: Person
pers =
    Person (HumanName "Big Bird") (DogName "Barkley") (Address "Sesame Street")

chris :: Person
chris =
    Person (HumanName "Chris Allen") (DogName "Papu") (Address "Austin")

-- Write methods without Reader
getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

-- With Reader
getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

-- With concrete Reader, instead of ambient Applicative / Monad
(<$->>) :: (a -> b) -> (r -> a) -> (r -> b)
(<$->>) = (<$>)

(<*->>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
(<*->>) = (<*>)

getDogR' :: Person -> Dog
getDogR' = Dog <$->> dogName <*->> address
```

```haskell
import Control.Applicative (liftA2)

-- With Reader
getDogR' :: Person -> Dog
getDogR' = liftA2 Dog dogName address
```

********** BEGIN EXERCISE: READING COMPREHENSION **********

See `ReadingComprehension.hs`.

********** END EXERCISE: READING COMPREHENSION **********

- The `Monad` of functions

```haskell
foo :: (Functor f, Num a) => f a -> f a
foo r = fmap (+1) r

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

-- If we wanted to combine both methods, we could do this
froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

-- Or we could combine the two methods. Rewrite 'bar' to accept one argument
-- instead of two:
barOne :: Foldable t => t a -> (t a, Int)
barOne r = (r, length r)

-- We could increment values in list as:
barPlus r = (foo r, length r)

-- But we could make it more compact by making (foo r) first argument to bar
frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

-- Make it more Readery
frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

-- Abstract to avoid dependency on 'foo' and 'bar' specific functions
-- 'm' = 'foo'
-- 'k' = 'bar'
--
-- Type signature may look like:
--
-- fooBind :: (t2 _> t1) -> (t1 -> t2 -> t) -> t2 -> t
fooBind m k = \r -> k (m r) r
```

- The `Monad` instance

- Example uses of the `Reader` type

```haskell
-- with Reader monad
getDogRM :: Person -> Dog
getDogRM = do
    name <- dogName
    addy <- address
    return $ Dog name addy
```

********** BEGIN EXERCISE: READER MONAD **********

See `ReaderMonad.hs`.

********** END EXERCISE: READER MONAD **********

- Reader Monad by itself is boring
    - See `PrettyReader.hs`.

- You can only change what comes below
    - You can swap different type/value for `r` for functions you call, but not
      for functions that call you.
    - `State` monad allows this.

```haskell
-- (r' -> r): The function to modify the environment.
--
-- (ReaderT r m a): Computation to run in the modified environment.
withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a

withReaderT f m = ReaderT $ runReaderT m . f
```

- You tend to see `ReaderT`, not `Reader`
    - `Reader` is one monad in a stack of multiple types providing monads
    - In context of multiple monadic contexts, you generally want/see monad
      transformers, which for `Reader` is `ReaderT`.

********** BEGIN CHAPTER EXERCISES **********

Warm-up

See `ReaderPractice.hs`.

********** END CHAPTER EXERCISES **********
