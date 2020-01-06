-- functors2.hs
module Functors2 where

data FixMePls a = FixMe | Pls a deriving (Eq, Show)

instance Functor FixMePls where
    fmap =
        error
        "it doesn't matter, it won't compile"
