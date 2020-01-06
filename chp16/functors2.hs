-- functors2.hs
module Functors2 where

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor (FixMePls a) where
    fmap _ FixMe = FixMe
    -- Note how `fmap` usage lines up well with its type signature:
    -- `fmap :: Functor f => (a -> b) -> f a -> f b`
    -- 'f' is 'Pls'
    -- 'a' is 'a'
    -- 'b' is '(f a)'
    fmap f (Pls a) = Pls (f a)
