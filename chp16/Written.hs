{-# LANGUAGE FlexibleInstances #-}
-- Written.hs
module Written where


-- 1)
data Quant a b = Finance | Desk a | Floor b

-- 2)
data K a b = K a

-- 3)
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a

instance Functor (Flip K a) where
    fmap = undefined

-- 4)
data EvilGoateeConst a b = GoatyConst b

-- 5)
data LiftItOut f a = LiftItout (f a)

-- 6)
data Parappa f g a = DaWrappa (f a) (g a)

-- 7)
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

-- 8)
data Notorious g o a t = Notorious (g o) (g a) (g t)

-- 9)
data List a = Nil | Cons a (List a)

-- 10)
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

-- 11)
data TalkToMe a = Halt | Print String a | Read (String -> a)
