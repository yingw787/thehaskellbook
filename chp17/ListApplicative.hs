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
    fmap = undefined

instance Applicative List where
    pure = undefined
    (<*>) = undefined

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
    Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f as = undefined
