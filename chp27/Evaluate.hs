-- Evaluate.hs
module Evaluate where


-- 1)
--
-- evaluates to 1
const 1 undefined

-- 2)
--
-- evaluates to bottom
const undefined 1

-- 3)
--
-- evaluates to 1
flip const undefined 1

-- 4)
--
-- evaluates to bottom
flip const 1 undefined

-- 5)
--
-- evaluates to bottom
const undefined undefined

-- 6)
--
-- evaluates to 'a'
foldr const 'z' ['a'..'e']

-- 7)
--
-- evaluates to 'z'
foldr (flip const) 'z' ['a'..'e']
