-- Evaluate.hs
module Evaluate where


-- 1)
const 1 undefined

-- 2)
const undefined 1

-- 3)
flip const undefined 1

-- 4)
flip const 1 undefined

-- 5)
const undefined undefined

-- 6)
foldr const 'z' ['a'..'e']

-- 7)
foldr (flip const) 'z' ['a'..'e']
