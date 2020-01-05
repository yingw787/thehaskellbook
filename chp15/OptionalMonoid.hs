-- OptionalMonoid.hs
module OptionalMonoid where

import Data.Monoid

-- Write the `Monoid` instance for our `Maybe` type renamed to `Optional`.
data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada

-- (FROM ANSWER KEY: https://stackoverflow.com/a/53800966)
--
-- As of Haskell base-4.11.0.0, `Semigroup` is a superclass of `Monoid`, and
-- method `(<>) / mappend` should be defined as part of the `Semigroup`
-- instance.
instance Semigroup a => Semigroup (Optional a) where
    Nada <> Nada = Nada
    Nada <> (Only a) = Only a
    (Only a) <> Nada = Only a
    (Only a) <> (Only a') = Only (a <> a')

-- (CORRECT BY GHCI OUTPUT)
main :: IO ()
main = do
    -- should be `Only (Sum {getSum = 2})
    print $ Only (Sum 1) `mappend` Only (Sum 1)
    -- should be `Only (Product {getProduct = 8})
    print $ Only (Product 4) `mappend` Only (Product 2)
    -- should be `Only (Sum {getSum = 1})`
    print $ Only (Sum 1) `mappend` Nada
    -- should be `Only [1]`
    print $ Only [1] `mappend` Nada
