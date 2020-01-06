-- functors1.hs
module Functors1 where

-- This definition of 'FixMePls' fails to compile:
--
-- Prelude> :l functors1.hs
-- [1 of 1] Compiling Functors1        ( functors1.hs, interpreted )

-- functors1.hs:6:18: error:
--     • Expected kind ‘* -> *’, but ‘FixMePls’ has kind ‘*’
--     • In the first argument of ‘Functor’, namely ‘FixMePls’
--       In the instance declaration for ‘Functor FixMePls’
--   |
-- 6 | instance Functor FixMePls where
--   |                  ^^^^^^^^
-- Failed, no modules loaded.
-- Prelude>
--
-- Without having a type argument / being higher-kinded, 'FixMePls' cannot be
-- used within a Functor. It would just apply the functor.
data FixMePls = FixMe | Pls deriving (Eq, Show)

instance Functor FixMePls where
    fmap =
        error
        "it doesn't matter, it won't compile"
