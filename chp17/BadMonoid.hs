-- BadMonoid.hs
--
-- In order to have module 'Test.QuickCheck.Checkers' available globally
-- (without 'stack.yaml'), run command:
--
-- `stack install checkers`
--
-- Using `checkers` v0.4.11
module BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary =
        frequency [ (1, return Fools), (1, return Twoo) ]

instance Semigroup Bull where
    (<>) _ _ = Fools

instance Monoid Bull where
    mempty = Fools
    mappend = (<>)

-- `EqProp` is from library `checkers`; `checkers` exports method `eq`.
instance EqProp Bull where
    (=-=) = eq


main :: IO ()
main = do
    -- Monoid laws defined as part of test batch `monoid`.
    --
    -- Passing a value of type to `monoid` so it knows which arbitrary instance
    -- to use. `monoid` doesn't actually use the value passed in here.
    --
    -- (PERSONAL NOTE: Seems rather odd to do it this way, why not pass in the
    -- type directly again...?)
    quickBatch (monoid Twoo)


-- Prelude> import Test.QuickCheck
-- Prelude Test.QuickCheck> import Test.QuickCheck.Checkers
-- Prelude Test.QuickCheck Test.QuickCheck.Checkers> import Test.QuickCheck.Classes
-- Prelude Test.QuickCheck Test.QuickCheck.Checkers Test.QuickCheck.Classes> :i quickBatch
-- quickBatch :: TestBatch -> IO ()
--         -- Defined in ‘Test.QuickCheck.Checkers’
-- Prelude Test.QuickCheck Test.QuickCheck.Checkers Test.QuickCheck.Classes> :i applicative
-- applicative ::
--   (Applicative m, Arbitrary a, CoArbitrary a, Arbitrary b,
--    Arbitrary (m a), Arbitrary (m (b -> c)), Show (m (b -> c)),
--    Arbitrary (m (a -> b)), Show (m (a -> b)), Show a, Show (m a),
--    EqProp (m a), EqProp (m b), EqProp (m c)) =>
--   m (a, b, c) -> TestBatch
--         -- Defined in ‘Test.QuickCheck.Classes’
-- Prelude Test.QuickCheck Test.QuickCheck.Checkers Test.QuickCheck.Classes> xs = [("b", "w", 1)]
-- Prelude Test.QuickCheck Test.QuickCheck.Checkers Test.QuickCheck.Classes> quickBatch $ applicative xs

-- applicative:
--   identity:     +++ OK, passed 500 tests.
--   composition:  +++ OK, passed 500 tests.
--   homomorphism: +++ OK, passed 500 tests.
--   interchange:  +++ OK, passed 500 tests.
--   functor:      +++ OK, passed 500 tests.

-- Prelude Test.QuickCheck Test.QuickCheck.Checkers Test.QuickCheck.Classes> type SSI = (String, String, Int)
-- Prelude Test.QuickCheck Test.QuickCheck.Checkers Test.QuickCheck.Classes> :{
-- Prelude Test.QuickCheck Test.QuickCheck.Checkers Test.QuickCheck.Classes| let trigger :: [SSI]
--
-- (NOTE: Usage of bottom / undefined indicates how quickBatch doesn't actually
-- use the values, it uses the types only)
--
-- Prelude Test.QuickCheck Test.QuickCheck.Checkers Test.QuickCheck.Classes|     trigger = undefined
-- Prelude Test.QuickCheck Test.QuickCheck.Checkers Test.QuickCheck.Classes| :}
-- Prelude Test.QuickCheck Test.QuickCheck.Checkers Test.QuickCheck.Classes> quickBatch (applicative trigger)

-- applicative:
--   identity:     +++ OK, passed 500 tests.
--   composition:  +++ OK, passed 500 tests.
--   homomorphism: +++ OK, passed 500 tests.
--   interchange:  +++ OK, passed 500 tests.
--   functor:      +++ OK, passed 500 tests.
-- Prelude Test.QuickCheck Test.QuickCheck.Checkers Test.QuickCheck.Classes>
