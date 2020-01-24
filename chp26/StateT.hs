-- StateT.hs
module StateT where


newtype StateT s m a = State { runStateT :: s -> m (a, s) }
