-- IsValid.hs
module IsValid where

import GHC.Arr


-- 1)
data Bool = False | True

-- 2)
data BoolAndSomethingElse a = False' a | True' a

-- 3)
data BoolAndMaybeSomethingElse a = Falsish | Truish a

-- 4)
newtype Mu f = InF { outF :: f (Mu f) }

-- 5)
data D = D (Array Word Word) Int Int
