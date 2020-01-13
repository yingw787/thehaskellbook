-- BadMonad.hs
module BadMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data CountMe a = CountMe Integer a deriving (Eq, Show)

instance Functor CountMe where
    -- (PERSONAL NOTE: I haven't finishing writing out everything yet but this
    -- looks to me like a bad functor...) (Yes it is a bad functor)
    --
    -- fmap f (CountMe i a) = CountMe (i + 1) (f a)
    --
    -- (WORKING)
    --
    fmap f (CountMe i a ) = CountMe i (f a)

instance Applicative CountMe where
    pure = CountMe 0
    -- (WORKING)
    --
    CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)
    --
    -- (NOT WORKING)
    --
    -- CountMe n f <*> CountMe _ a = CountMe (n + 1) (f a)

instance Monad CountMe where
    return = pure
    --
    -- (NOT WORKING)
    --
    -- CountMe n a >>= f =
    --     let CountMe _ b = f a
    --     in CountMe (n + 1) b
    --
    -- (STILL NOT WORKING, DOES NOT SATISFY RIGHT IDENTITY)
    --
    CountMe _ a >>= f = f a

instance Arbitrary a => Arbitrary (CountMe a) where
    arbitrary = CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
    (=-=) = eq

main = do
    let trigger :: CountMe (Int, String, Int)
        trigger = undefined

    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
