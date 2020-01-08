-- ConstantInstance.hs
--
-- (PERSONAL NOTE: Honestly, first pass around, I'm not quite sure how to work
-- through this. All diffs are from answer key.)
module ConstantInstance where


newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    -- (FROM ANSWER KEY)
    --
    -- (PERSONAL NOTE: 'f' is not applied, this is why 'Constant' is used when
    -- you need to elide a function application)
    fmap f (Constant a) = (Constant a)

instance Monoid a => Applicative (Constant a) where
    -- (FROM ANSWER KEY)
    --
    -- (PERSONAL NOTE: 'mempty' appears to be a sane default for an arbitrary
    -- type argument, to be defined in a concrete type somewhere else. I thought
    -- it was tightly coupled to Monoid for some reason...) (I still might be
    -- wrong...)
    pure f = (Constant mempty)
    --
    -- (I tried to implement (<*>), this is incorrect)
    --
    -- (<*>) (Constant f) (Constant a) = (Constant a)
    --
    (<*>) (Constant a) (Constant b) = Constant (mappend a b)
