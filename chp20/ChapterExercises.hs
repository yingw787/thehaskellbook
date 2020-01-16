-- ChapterExercises.hs
module ChapterExercises where

import Data.Foldable
import Data.Monoid


-- 1)
data Constant a b = Constant b
-- (PERSONAL NOTE: I know I need to implement `foldr` or `foldMap` but I don't
-- know where to start...) (Hmm looking at my notes from this chapter
-- helps...lemme give it a shot...)

-- instance Foldable (Constant a) where
--     foldr f z (Constant b) = f b z
--     foldl f z (Constant b) = f z b
--     foldMap f (Constant b) = f b
--
-- (INCORRECT BY GHCI OUTPUT; VALUES SHOULD BE HELD CONSTANT)
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
instance Foldable (Constant a) where
    foldMap _ _ = mempty

-- 2)
data Two a b = Two a b deriving Show

-- (CORRECT BY CHECKING ANSWER KEY)
instance Foldable (Two a) where
    foldMap f (Two a b) = f b

-- 3)

-- 4)

-- 5)


main :: IO ()
main = do
    print $ foldr (*) 5 (Constant 5)

    print $ foldMap Sum (Two 5 5)
