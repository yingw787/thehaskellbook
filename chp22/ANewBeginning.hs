-- ANewBeginning.hs
module ANewBeginning where

import Control.Applicative


boop = (*2)
doop = (+10)

-- Will be monomorphic if loaded from a file, polymorphic would involve changing
-- the type signature.
bip :: Integer -> Integer
bip = boop . doop

-- You can `fmap` a function over another function. Functorial context is a
-- partially applied function.
--
-- Example: `fmap boop doop x == (*2) ((+10) x)`
bloop :: Integer -> Integer
bloop = fmap boop doop

bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop
