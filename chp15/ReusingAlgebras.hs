-- ReusingAlgebras.hs
module ReusingAlgebras where

import Data.Monoid;

-- Give you a new monoid type for a larger type through reuse of smaller
-- monoids.
--
-- This is important when possible values of an encapsulating type don't include
-- value of type argument (e.g. `Nothing` vs. `Just x`).
--
-- The definitions of `Monoids` are included within `GHC.Base`. The following
-- three lines of code are commented out otherwise this file won't load.
--
-- instance Monoid b => Monoid (a -> b)
-- instance (Monoid a, Monoid b) => Monoid (a, b)
-- instance (Monoid a, Monoid b, Monoid c) => Monoid (a, b, c)
--
-- If a datatype has a phantom type, the typechecker does not demand a `Monoid`
-- instance for that argument.
data Booly a = False' | True' deriving (Eq, Show)

-- conjunction
instance Semigroup (Booly a) where
    (<>) False' _ = False'
    (<>) _ False' = False'
    (<>) True' True' = True'

-- We don't need a monoidal constraint for `a` because we're never combining `a`
-- values (none exist), and we're never asking for `mempty` for type `a`.
instance Monoid (Booly a) where
    mempty = True'
