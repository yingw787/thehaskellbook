-- IterateAndUnfoldr.hs
module IterateAndUnfoldr where

import Data.List -- `unfoldr`

-- 1)
myIterate :: (a -> a) -> a -> [a]
-- (Not sure how to proceed; succ x can only be done if x is enumerable, which
-- is not determined by the type signature)
--
-- myIterate f x = [x] ++ (myIterate f (succ x))
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
-- (PERSONAL NOTE: You can reapply a function to a value in order to get the
-- "successor" of that value..)
myIterate f x = x : myIterate f (f x)

-- 2)
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
-- (I'm not sure how to pattern match on a value that does not exist, because it
-- is generated from a combination of function inputs.)
--
-- (Use a `go` method in order to have an incremental variable, and create a new
-- variable by applying the function input argument to the other input
-- argument.)
myUnfoldr f x = go f x (f x) where
    go f s Nothing = []
    go f s (Just (a, b)) = a : go f b (f b)

-- 3)
betterIterate :: (a -> a) -> a -> [a]
-- (Yeah I'm kind of confused here...)
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
-- (PERSONAL NOTE: Still quite weak on typeclass Foldable.)
betterIterate f x = myUnfoldr (g f) s where
    g f s = Just (s, (f s))
