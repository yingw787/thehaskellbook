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
