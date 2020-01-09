-- Apl1.hs
module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


-- this isn't going to work properly
instance Monoid a => Semigroup (ZipList a) where
    (<>) = liftA2 mappend

instance Monoid a => Monoid (ZipList a) where
    -- This won't work because the empty `ZipList` is the zero and not the
    -- identity!
    --
    -- mempty = ZipList []
    --
    -- Change to:
    --
    mempty = pure mempty
    mappend = (<>)

-- (PERSONAL NOTE: Not sure how the author got this to compile, but I get a
-- namespacing error when importing module 'Test.QuickCheck' because it collides
-- with arbitrary instance declaration for 'ZipList'...)
--
-- (PERSONAL NOTE: I checked the answer key from
-- https://github.com/johnchandlerburnham/hpfp/ and these portions of the module
-- are commented out, so I will comment them out)
--
-- instance Arbitrary a => Arbitrary (ZipList a) where
--     arbitrary = ZipList <$> arbitrary

-- instance Arbitrary a => Arbitrary (Sum a) where
--     arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where
    (=-=) = eq
