-- brokenMaybe1.hs
module BrokenMaybe1 where

-- Can't fully resolve ambiguous imports and definitions.
--
-- data Maybe_ a = Nothing | Just a
--
-- f :: Bool -> Maybe_ Int
-- f False = Prelude.Just 0
-- f _ = Prelude.Nothing

-- Get a type error when attempting to compile.
--
-- f :: Bool -> Maybe Int
-- f False = 0 :: Int
-- f _ = Nothing
