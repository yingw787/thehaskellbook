-- ConstantInstance.hs
module ConstantInstance where


newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap = undefined

instance Monoid a => Applicative (Constant a) where
    pure = undefined
    (<*>) = undefined
