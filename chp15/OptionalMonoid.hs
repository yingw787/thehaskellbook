-- OptionalMonoid.hs
module OptionalMonoid where

import Data.Monoid

-- Write the `Monoid` instance for our `Maybe` type renamed to `Optional`.
data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    mappend Nada Nada = Nada
    mappend Nada (Only x) = (Only x)
    mappend (Only x) Nada = (Only x)
    mappend (Only x) (Only y) = (Only (mappend x y))

main :: IO ()
main = do
    print "Hello"
    -- print $ Only (Sum 1) `mappend` Only (Sum 1)
