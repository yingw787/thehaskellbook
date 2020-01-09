-- TypeSpecializations.hs
module TypeSpecializations where


-- 1)
--
-- (PERSONAL NOTE: ...so I have it in my notes, but I'm not sure how to declare
-- the Applicative instance seeing how it is probably in the Prelude already.)
--
-- pure :: a -> [a]
-- (<*>) :: [(a -> b)] -> [a] -> [b]
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
listPure :: a -> [a]
listPure = pure

listApply :: [(a -> b)] -> [a] -> [b]
listApply = (<*>)

-- 2)
--
-- (CORRECT BY CHECKING ANSWER KEY)
ioPure :: a -> IO a
ioPure = pure

ioApply :: IO (a -> b) -> IO a -> IO b
ioApply = (<*>)

-- 3)
--
-- (INCORRECT, COMPLIATION ERROR)
--
-- tuplePure :: a -> (,) a
-- tuplePure = pure
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
-- (PERSONAL NOTE: I don't understand this solution. I thought the whole point
-- of 'Control.Applicative' was that the function would be applied to the
-- arguments (monoidal)? Why is it present in the final result?)
--
tuplePure :: (Monoid a, Monoid c) => c -> (a, c)
tuplePure = pure

-- (PERSONAL NOTE: Not sure how to reduce the kind-ness of (,).)
--
-- (INCORRECT, GHC COMPILATION ERROR)
--
-- tupleApply :: (,) (a -> b) -> (,) a -> (,) b
-- tupleApply = (<*>)
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
-- (PERSONAL NOTE: I think 'b' doesn't need the Monoid constraint because only a
-- needs to be combinable.)
--
tupleApply :: (Monoid a, Monoid c) => (c, (a -> b)) -> (c, a) -> (c, b)
tupleApply = (<*>)

-- 4)
--
-- functionalPure :: a -> (-> a)
--
-- (PERSONAL NOTE: I checked ':t (+1)' as a partially applied function and this
-- was the type signature)
--
-- functionalPure :: a -> (a -> a)
--
-- (FROM ANSWER KEY)
functionalPure :: a -> (e -> a)
functionalPure = pure

-- functionalApply :: ((a -> b) -> (a -> b)) -> (a -> a) -> (b -> b)
--
-- (FROM ANSWER KEY)
functionalApply :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
functionalApply = (<*>)
