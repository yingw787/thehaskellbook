-- Check.hs
--
-- If we wanted to check the property of associativity given types and values,
-- it may look like this:
--
-- 1 + (2 + 3) == (1 + 2) + 3
-- 4 * (5 * 6) == (4 * 5) * 6
--
-- For abstract types / values and specific methods, you could use lambda
-- calculus:
--
-- \ a b c -> a + (b + c) == (a + b) + c
-- \ a b c -> a * (b * c) == (a * b) * c
--
-- For abstract method 'f':
--
-- \ f a b c -> f a (f b c) == f (f a b) c
--
-- We can use QuickCheck in order to run a generative test to weakly assert the
-- property of associativity for our function and given concrete types. Weak
-- assertions since generative tests cannot prove that a property holds for all
-- values.
module Check where


import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
