{-# LANGUAGE NoMonomorphismRestriction #-}
-- MatchTheTypes.hs
module MatchTheTypes where

-- (1)
-- i :: Num a => a
-- i = 1
--
-- i :: a
-- i = 1

-- (2)
-- f :: Float
-- f = 1.0
--
-- f :: Num a => a
-- f = 1.0

-- (3)
-- f :: Float
-- f = 1.0
--
-- f :: Fractional a => a
-- f = 1.0