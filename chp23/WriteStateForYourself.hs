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
