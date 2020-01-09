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
