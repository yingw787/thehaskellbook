-- QuickCheck.hs
module QuickCheck where

import Test.QuickCheck

-- 1)
half x = x / 2
halfIdentity = (*2) . half

-- (CORRECT BY GHCI OUTPUT)
-- (PERSONAL NOTE: I think I might have overconstrained this property method by
-- using concrete types for my type signature. I'm not sure how to set concrete
-- types within the quickCheck command in the do notation itself.)
prop_halfIdentityHolds :: Double -> Bool
prop_halfIdentityHolds x = (x == halfIdentity x)

runOne :: IO ()
runOne = do
    quickCheck prop_halfIdentityHolds

-- (ANSWER KEY BELOW: https://github.com/johnchandlerburnham/hpfp)
halfIdentity' :: Fractional a => a -> a
halfIdentity' = (*2) . half

prop_twiceHalf :: (Eq a, Fractional a) => a -> Bool
prop_twiceHalf n = (halfIdentity' n) == n

runOne' :: IO ()
runOne' = do
    quickCheck (prop_twiceHalf :: Double -> Bool)
