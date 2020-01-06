-- Failure.hs
module Failure where

import Control.Monad
import Data.Monoid
import Test.QuickCheck
import Check (monoidAssoc, monoidLeftIdentity, monoidRightIdentity)

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [(1, return Fools), (1, return Twoo)]

instance Semigroup Bull where
    (<>) _ _ = Fools

-- This semigroup / monoid combination fails the test of associativity, since
-- identity combined with non-identity results in identity value instead of
-- non-identity value.
instance Monoid Bull where
    mempty = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool


main :: IO ()
main = do
    let ma = monoidAssoc
        mli = monoidLeftIdentity
        mri = monoidRightIdentity

    quickCheck (ma :: BullMappend)
    quickCheck (mli :: Bull -> Bool)
    quickCheck (mri :: Bull -> Bool)
