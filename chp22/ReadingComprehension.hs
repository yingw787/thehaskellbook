-- ReadingComprehension.hs
{-# LANGUAGE InstanceSigs #-}
module ReadingComprehension where


newtype Reader r a = Reader { getReader :: r -> a }

-- 1)
myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 = undefined

-- 2)
asks :: (r -> a) -> Reader r a
asks f = undefined

-- 3)
instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = undefined

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (<*>) (Reader rab) (Reader ra) = undefined
