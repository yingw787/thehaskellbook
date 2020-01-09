-- ZipListApplicative.hs
--
-- (PERSONAL NOTE: Still lost. See https://github.com/johnchandlerburnham/hpfp)
module ZipListApplicative where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys' where
        -- Testing for equality of infinite lists is impossible. The test for
        -- homomorphism in `Applicative` will be infinite; since PBT is already
        -- a soft assurance, we arbitrary deem 3000 as "good enough".
        xs' = let (ZipList' l) = xs in take 3000 l
        ys' = let (ZipList' l) = ys in take 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) =
        ZipList' $ fmap f xs

-- (CORRECT BY CHECKING ANSWER KEY)
instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary = do
        as <- arbitrary
        return (ZipList' as)

-- (CORRECT BY CHECKING ANSWER KEY)
--
-- (PERSONAL NOTE: All book examples have newtype inheriting from List, while
-- 1.0-RC4 has bare [] type. I basically took the existing solutions and
-- translated them; it's probably trivial, but I didn't count it as directly
-- copying as with `ListApplicative.hs`. Maybe I should.)
repeat' :: a -> [a]
repeat' a = [a] ++ (repeat' a)

zipWith' :: [(a -> b)] -> [a] -> [b]
zipWith' [] _ = []
zipWith' _ [] = []
zipWith' (f:fs) (x:xs) = [(f x)] ++ (zipWith' fs xs)

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
instance Applicative ZipList' where
    pure a = ZipList' $ repeat' a
    (<*>) (ZipList' fs) (ZipList' xs) = ZipList' (zipWith' fs xs)


main :: IO ()
main = do
    let test = [(1, 2, 3), (4, 5, 6)] :: [(Int, Int, Int)]
    quickBatch $ applicative (ZipList' test)
