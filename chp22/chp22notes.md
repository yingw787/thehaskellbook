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
