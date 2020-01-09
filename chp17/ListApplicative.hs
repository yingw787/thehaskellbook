-- ListApplicative.hs
--
-- (PERSONAL NOTE: I am quite literally lost at this point. Assume all notes are
-- from answer key here: https://github.com/johnchandlerburnham/hpfp. Marking
-- chapter as to review later.) (Looks like I already did that lol)
module ListApplicative where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    fmap _ Nil = Nil
    -- (PERSONAL NOTE: If I have to explain this in my own words, 'Cons a as'
    -- pattern matches on a defined, non-empty List type, where the result is
    -- still non-empty, 'f' is applied to single value 'a', and 'f' is applied
    -- to list tail 'as' using method 'fmap', which is lifted into Functorial
    -- structure 'Cons'.)
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Applicative List where
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    --
    -- (PERSONAL NOTE: Explained in my own words, 'pure' lifts a value into
    -- functorial structure, which for a defined value would be 'Cons'. Since no
    -- other values are known, the value 'Nil' is appended as the tail.)
    pure x = Cons x Nil
    (<*>) _ Nil = Nil
    (<*>) Nil _ = Nil
    -- (PERSONAL NOTE: I'm not quite sure how to explain the two lines below.)
    (<*>) (Cons f fs) as = (fmap f as) `append` ((<*>) fs as)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = do
        as <- arbitrary
        return (mkList as)

instance Eq a => EqProp (List a) where
    (=-=) = eq

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

mkList :: [a] -> List a
mkList xs = foldr Cons Nil xs


main :: IO ()
main = do
    let test = [(1, 2, 3), (4, 5, 6)] :: [(Int, Int, Int)]
    -- (PERSONAL NOTE: Holy crap 'composition' took quite a while...)
    quickBatch $ applicative (mkList test)
