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

-- `EqProp` is from library `checkers`
instance EqProp Bull where
    (=-=) = eq


main :: IO ()
main = do
    quickBatch (monoid Twoo)
