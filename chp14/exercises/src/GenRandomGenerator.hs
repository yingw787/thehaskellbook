-- GenRandomGenerator.hs
module GenRandomGenerator where

import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

data Fool = Fulse | Frue deriving (Eq, Show)

-- 1) Equal probabilities for each.
--
-- (INCORRECT, COMPILE-TIME ERROR)
--
-- sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
-- sumGenEqual = do
--     a <- arbitrary
--     b <- arbitrary
--     oneof [return $ First a, return $ Second b]

-- sumGenFoolFool :: Gen (Sum Fool Fool)
-- sumGenFoolFool = sumGenEqual

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
-- (PERSONAL NOTE: Not quickCheck foolGen, but rather sample foolGen)
instance Arbitrary Fool where
    arbitrary = frequency [(1, return Fulse), (1, return Frue)]

foolGen :: Gen Fool
foolGen = arbitrary

-- 2) 2/3s chance of Fulse, 1/3 chance of Frue.
--
-- (FROM ANSWER KEY)
fulsishFoolGen :: Gen Fool
fulsishFoolGen = do
    a <- arbitrary
    frequency [(2, return a), (1, return a)]
