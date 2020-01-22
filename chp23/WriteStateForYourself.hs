-- WriteStateForYourself.hs
{-# LANGUAGE InstanceSigs #-}
module WriteStateForYourself where


newtype Moi s a = Moi { runMoi :: s -> (a, s) }

-- Implement the 'Functor' instance for 'State'.
instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    -- fmap f (Moi g) = Moi (f g)
    --
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    --
    fmap f (Moi g) = Moi $ \s0 -> let (a, s1) = (g s0) in (f a, s1)

-- Implement the 'Applicative' instance for 'State'.
instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = undefined

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (<*>) (Moi f) (Moi g) = undefined

-- Implement the 'Monad' instance for 'State'.
instance Monad (Moi s) where
    return = pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (>>=) (Moi f) g = undefined
