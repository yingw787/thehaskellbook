-- EitherT.hs
module EitherT where


newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }
