-- IdentityInstance.hs
module IdentityInstance where

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    -- fmap = id
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    -- pure = id
    -- x <*> Identity = x
    -- Identity <*> x = x
    --
    -- (INCORRECT, I'm not exactly sure why)
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    --
    -- (PERSONAL NOTE: I think that 'Identity', because there is only one data
    -- constructor, provides the consistent functorial structure to pass through
    -- any methods to be applied to the internal value)
    pure f = (Identity f)
    (<*>) (Identity f) (Identity a) = Identity (f a)
