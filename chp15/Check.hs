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
--
-- In order to have module `Test.QuickCheck` available globally (i.e. without
-- `stack.yaml`), run command(s):
--
-- stack install QuickCheck
--
-- Using QuickCheck v2.11.3 (global)
module Check where


import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a


main :: IO ()
main = do
    quickCheck (monoidAssoc :: String -> String -> String -> Bool)
    --
    -- Run `verboseCheck monoidAssoc` in `stack ghci` to see results of not
    -- casting to concrete types and GHCi's type-defaulting behavior. If
    -- compiled, `stack ghc` would complain about lack of type constraints.
    quickCheck (monoidLeftIdentity :: String -> Bool)
    quickCheck (monoidRightIdentity :: String -> Bool)
