-- dividedBy.hs
--
-- PARTIAL IMPLEMENTATION FROM ANSWER KEY:
-- https://github.com/johnchandlerburnham/hpfp
module DividedBy where

data DividedResult =
    Result Integer
    -- NOTE: really important that data constructor `DividedResult` derives from
    -- (Eq, Show)!
    --
    -- Equivalent to Maybe Integer
    | DividedByZero deriving (Eq, Show)

-- NOTE: Not (DividedResult, DividedResult) because this is not `divMod`.
-- Instead, return one value of type `DividedResult`.
-- Wrap guard syntax within a go function in order to have the counter available
-- in the inner method.
--
-- `resNeg` negates the eventual result.
dividedByFixed :: Integral a => a -> a -> DividedResult
-- NOTE: A compile-time error saying an error occurred in the `where` clause was
-- actually a `(==)` equality comparison operator in place of `(=)` variable
-- assignment operator.
dividedByFixed num denom = go num denom 0
    where
        resNeg (Result x) = Result (negate x)
        go n d count
            | d == 0 = DividedByZero
            | d < 0 = resNeg $ go n (negate d) count
            | n < 0 = resNeg $ go (negate n) d count
            | n < d = Result count
            | otherwise = go (n - d) d (count + 1)

main :: IO ()
main = do
    print (dividedByFixed 0 9)
