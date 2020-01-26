-- StateT.hs
--
-- (UNLESS OTHERWISE NOTED, ANSWERS FROM ANSWER KEY: )
module StateT where


newtype StateT s m a = State { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
    fmap f m = undefined

instance Monad m => Applicative (StateT s m) where
    pure = undefined
    (<*>) = undefined

instance Monad m => Monad (StateT s m) where
    return = pure
    (>>=) sma f = undefined
