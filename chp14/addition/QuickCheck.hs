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

runQc :: IO ()
runQc = quickCheck prop_additionGreater
