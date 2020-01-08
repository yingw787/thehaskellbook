-- IsValid.hs
module IsValid where

import GHC.Arr


-- 1)
--
-- No, a valid Functor instance cannot be written, because Bool has kind '*'.
--
-- (CORRECT BY CHECKING ANSWER KEY)
data Bool = False | True

-- 2)
--
-- Yes, a valid Functor instance can be written, kind is '* -> *'.
--
-- (CORRECT BY CHECKING ANSWER KEY)
data BoolAndSomethingElse a = False' a | True' a

-- 3)
--
-- Yes, kind is '* -> *'.
--
-- (CORRECT BY CHECKING ANSWER KEY)
data BoolAndMaybeSomethingElse a = Falsish | Truish a

-- 4)
--
-- Yes, kind is '(* -> *) -> *'.
--
-- (CORRECT BY CHECKING ANSWER KEY)
newtype Mu f = InF { outF :: f (Mu f) }

-- 5)
--
-- No, kind is '*'.
--
-- (CORRECT BY CHECKING ANSWER KEY)
data D = D (Array Word Word) Int Int
