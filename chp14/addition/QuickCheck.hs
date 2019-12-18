-- QuickCheck.hs
module QuickCheck where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

-- Failure case
--
-- Will always fail on the first case to be tested.
-- prop_additionGreater' :: Int -> Bool
-- prop_additionGreater' x = x + 0 > x

runQc :: IO ()
runQc = do
    quickCheck prop_additionGreater
    -- quickCheck prop_additionGreater'
