-- ReaderMonad.hs
{-# LANGUAGE InstanceSigs #-}
module ReaderMonad where


newtype Reader r a = Reader { getReader :: r -> a }

-- From chp22/ReadingComprehension.hs
instance Functor (Reader r) where
    fmap f (Reader g)= Reader $ f . g

instance Applicative (Reader r) where
    pure :: a -> Reader r a
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    (<*>) (Reader rab) (Reader ra) = Reader $ \r -> rab r (ra r)

-- 1)
instance Monad (Reader r) where
    return = pure
    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    -- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    (>>=) (Reader ra) (aRb) = Reader $ \r -> getReader (aRb (ra r)) r


-- 2)
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
-- getDogRM' :: Reader Person Dog
-- getDogRM' = Reader $ Dog <$> dogName <*> address
