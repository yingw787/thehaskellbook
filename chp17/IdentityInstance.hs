-- IdentityInstance.hs
module IdentityInstance where

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap = undefined

instance Applicative Identity where
    pure = undefined
    (<*>) = undefined
