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
