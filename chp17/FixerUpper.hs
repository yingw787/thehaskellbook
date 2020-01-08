-- FixerUpper.hs
module FixerUpper where

-- 1)
-- const <$> Just "Hello" <*> "World"
--
-- (PERSONAL NOTE: Kind of just flailing around here)
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
-- (PERSONAL NOTE: `pure` lifts a value into functorial space, in this case
-- `Maybe`.)
--
const <$> Just "Hello" <*> (pure "World")

-- 2)
(,,,) Just 90 <*> Just 10 Just "Tierness" [1, 2, 3]
--
-- (PERSONAL NOTE: I think the final result should look something like `[(Just
-- 90, 1), (Just 10, 2), (Just "Tierness", 3)]`, but I'm not sure.) (Could also
-- be ([Just 90, 1], [Just 10, 2], [Just "Tierness", 3]) because of the '(,,,)',
-- but again, not sure.)
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
-- (PERSONAL NOTE: Wow, I predicted that answer completely wrong. It's 'Just
-- (90, 10, "Tierness", [1,2,3])'. Marked chapter as need to review further.)
--
(,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]
