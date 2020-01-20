-- ReaderMonad.hs
{-# LANGUAGE InstanceSigs #-}
module ReaderMonad where


newtype Reader r a = Reader { getReader :: r -> a }

instance Monad (Reader r) where
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (>>=) (Reader ra) (aRb) = undefined
