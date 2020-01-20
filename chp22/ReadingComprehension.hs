-- ReadingComprehension.hs
--
-- (PERSONAL NOTE: Falling behind roadmap schedule, this isn't immediately
-- obvious to me, leaning answer key)
{-# LANGUAGE InstanceSigs #-}
module ReadingComprehension where


newtype Reader r a = Reader { getReader :: r -> a }

-- 1)
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
myLiftA2 f x y = f <$> x <*> y

-- 2)
asks :: (r -> a) -> Reader r a
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
asks f = Reader f

-- 3)
instance Functor (Reader r) where
    fmap f (Reader g)= Reader $ f . g

instance Applicative (Reader r) where
    pure :: a -> Reader r a
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    pure a = Reader $ const a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
    (<*>) (Reader rab) (Reader ra) = Reader $ \r -> rab r (ra r)
