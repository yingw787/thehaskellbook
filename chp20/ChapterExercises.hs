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
--
-- (CORRECT BY CHECKING ANSWER KEY)
-- (PERSONAL NOTE: I think what I'm getting is higher-kinded types)
data Three a b c = Three a b c deriving Show

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

-- 4)
data Three' a b = Three' a b b deriving Show

-- (CORRECT BY CHECKING ANSWER KEY, GHCI OUTPUT AND COMPILE SUCCESS)
-- (PERSONAL NOTE: I was extremely surprised that this worked. I'm not sure why
-- but it just felt like the right solution. I don't think I understand this
-- intuitvely.)
instance Foldable (Three' a) where
    foldMap f (Three' a b c) = (<>) (f b) (f c)

-- 5)
data Four' a b = Four' a b b b deriving Show

-- (CORRECT BY CHECKING ANSWER KEY, GHCI OUTPUT, AND COMPILE SUCCESS)
instance Foldable (Four' a) where
    foldMap f (Four' a b c d) = (f b) <> (f c) <> (f d)


main :: IO ()
main = do
    print $ foldr (*) 5 (Constant 5)

    print $ foldMap Sum (Two 5 5)

    print $ foldMap Sum (Three 1 2 3)

    print $ foldMap Sum (Three' 1 2 3)

    print $ foldMap Sum (Four' 1 2 3 4)
