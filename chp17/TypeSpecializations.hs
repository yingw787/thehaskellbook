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
