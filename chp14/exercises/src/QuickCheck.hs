-- QuickCheck.hs
module QuickCheck where

import Data.List (sort)
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

-- 2)
--
-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
    snd $ foldr go (Nothing, True) xs
    where   go _ status@(_, False) = status
            go y (Nothing, t) = (Just y, t)
            go y (Just x, t) = (Just y, x >= y)

-- (CORRECT BY GHCI OUTPUT)
-- (PERSONAL NOTE: Answer key doesn't check whether it is true, it should be
-- unnecessary since QuickCheck does it for you. Just use pointfree notation.)
prop_listOrdered :: (Ord a) => [a] -> Bool
prop_listOrdered x = (listOrdered . sort) x == True

runTwo :: IO ()
runTwo = do
    quickCheck (prop_listOrdered :: [Int] -> Bool)

-- 3)
plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

-- (CORRECT BY GHCI OUTPUT)
runThree :: IO ()
runThree = do
    quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
    quickCheck (plusCommutative :: Int -> Int -> Bool)

-- 4)
plusAssociative' :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative' x y z = x * (y * z) == (x * y) * z

plusCommutative' :: (Num a, Eq a) => a -> a -> Bool
plusCommutative' x y = x * y == y * x

-- (CORRECT BY GHCI OUTPUT)
runFour :: IO ()
runFour = do
    quickCheck (plusAssociative' :: Int -> Int -> Int -> Bool)
    quickCheck (plusCommutative' :: Int -> Int -> Bool)

-- 5)
--
prop_quotRem :: Integral a => a -> a -> Bool
prop_quotRem x y = (quot x y) * y + (rem x y) == x

prop_divMod :: Integral a => a -> a -> Bool
prop_divMod x y = (div x y) * x + (mod x y) == y

-- (INCORRECT, DIVIDE BY ZERO ERROR)
--
-- runFive :: IO ()
-- runFive = do
--     quickCheck (prop_quotRem :: Int -> Int -> Bool)
--     quickCheck (prop_divMod :: Int -> Int -> Bool)
--
-- (ANSWER KEY DEFAULTS ZERO DIVISOR TO BE TRUE)

-- 6)
--
-- Is (^) associative?
--
-- No, quickCheck says (0, 0, 0) disproves power associativity.
powerAssociative :: (Integral a, Num a) => a -> a -> a -> Bool
powerAssociative x y z = (x ^ y) ^ z == x ^ (y ^ z)

-- Is (^) commutative?
--
-- No, quickCheck says (0, 1) disproves power commutativity.
powerCommutative :: (Integral a, Num a) => a -> a -> Bool
powerCommutative x y = x ^ y == y ^ x

runSix :: IO ()
runSix = do
    quickCheck (powerAssociative :: Int -> Int -> Int -> Bool)
    quickCheck (powerCommutative :: Int -> Int -> Bool)

-- 7)
prop_twiceReverseIsId :: Eq a => [a] -> Bool
prop_twiceReverseIsId x = (reverse . reverse) x == id x

runSeven :: IO ()
runSeven = do
    quickCheck (prop_twiceReverseIsId :: [Int] -> Bool)

-- 8)
--
-- ($)
-- (PERSONAL NOTE: Not sure where to start with this...how can quickCheck come
-- up with its own defnition of a function?)
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
-- (PERSONAL NOTE: Parentheses are important because ($) will halt until the
-- entire right-hand side of the expression is evaluated, while only the next
-- argument needs to be applied in this case.)
propApply :: (Eq b) => (a -> b) -> a -> Bool
propApply f a = (f $ a) == (f a)

-- (GHC error: couldn't match expected types)
--
-- propCompose :: (Eq c) => (a -> b) -> (b -> c) -> a -> Bool
-- propCompose f g x = (f . g) x == f (g x)
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
-- (PERSONAL NOTE: Got the order of function application incorrect)
--
prop_compose :: (Eq c) => (b -> c) -> (a -> b) -> a -> Bool
prop_compose f g x = (f . g) x == f (g x)

-- Add a specific typeclass for type signature (a -> b) in order to print
-- results from main block; otherwise at least GHCi will throw an error.
instance Show (a -> b) where show _ = "Arbitrary Function"

runEight :: IO ()
runEight = do
    -- (PERSONAL NOTE: Enable QuickCheck to come up with its own methods using
    -- the concrete type signature for a method, it is no different from a
    -- value.)
    quickCheck (propApply :: (Int -> Int) -> Int -> Bool)
    quickCheck (prop_compose :: (Int -> Int) -> (Int -> Int) -> Int -> Bool)

-- 9)
prop_foldrCons :: (Eq a) => [a] -> [a] -> Bool
-- (BELOW RESULTS IN QUICKCHECK FAILURE)
prop_foldrCons x y = foldr (:) x y == (++) x y

-- (Both of these should work)
prop_foldrCons' x y = foldr (:) x y == (++) y x
prop_foldrCons'' x y = foldr (:) x y == (flip (++)) x y

-- -- prop_foldrList ::
-- prop_foldrList x = foldr (:) [] x == concat x
--
-- (FROM ANSWER KEY)
prop_compareConcat :: (Eq a, Foldable t) => t [a] -> Bool
prop_compareConcat xs = foldr (++) [] xs == concat xs

runNine :: IO ()
runNine = do
    quickCheck (prop_foldrCons :: [Char] -> [Char] -> Bool)
    quickCheck (prop_compareConcat :: [String] -> Bool)

-- 10)
--
-- (QUICKCHECK DISPROVES WITH n = 1 and xs = "" as take takes n or length of
-- string, whichever comes first.)
prop_takeIsLength :: Int -> [a] -> Bool
prop_takeIsLength n xs = length (take n xs) == n

runTen :: IO ()
runTen = do
    quickCheck (prop_takeIsLength :: Int -> String -> Bool)

-- 11)
prop_readShowIdentity :: (Eq a, Read a, Show a) => a -> Bool
prop_readShowIdentity x = (read (show x)) == x

runEleven :: IO ()
runEleven = do
    quickCheck (prop_readShowIdentity :: String -> Bool)
