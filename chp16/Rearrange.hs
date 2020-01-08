-- Rearrange.hs
module Rearrange where


-- 1)
data Sum a b = First a | Second b

-- instance Functor (Sum e) where
--     fmap f (First a) = First (f a)
--     fmap f (Second b) = Second b
--
-- I don't think the above Functor declaration works because prior arguments are
-- locked while the function is only passed to the last data argument.
--
-- instance Functor (Sum e) where
--     fmap _ (First a) = First a
--     fmap f (Second b) = Second (f b)
--
-- (INCORRECT BY CHECKING ANSWER KEY)
-- (PERSONAL NOTE: Didn't pay attention to the instructions, need to vary the
-- ordering of the type arguments, don't redeclare the functor)
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
data Sum' b a = First' a | Second' b

instance Functor (Sum' e) where
    fmap f (First' a) = First' (f a)
    fmap f (Second' b) = Second' b

-- 2)
data Company a b c = DeepBlue a c | Something b

-- instance Functor (Company e e') where
--     fmap f (Something b) = Something (f b)
--     fmap _ (DeepBlue a c) = DeepBlue a c
--
-- I don't think the above Functor declaration works because a and b should be
-- pass-through w.r.t. f, while only c is affected.
--
-- instance Functor (Company a b) where
--     fmap _ (Something b) = Something b
--     fmap f (DeepBlue a c) = DeepBlue a (f c)
--
-- (INCORRECT BY CHECKING ANSWER KEY, didn't pay attention and changed the
-- Functor declaration instead of rearranging the type arguments)
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
data Company' a c b = DeepBlue' a c | Something' b

instance Functor (Company' e e') where
    fmap f (Something' b) = Something' (f b)
    fmap _ (DeepBlue' a c) = DeepBlue' a c

-- 3)
data More a b = L a b a | R b a b deriving (Eq, Show)

-- instance Functor (More x) where
--     fmap f (L a b a') = L (f a) b (f a')
--     fmap f (R b a b') = R b (f a) b'
--
-- This Functor declaration varies both a and b, it should only vary b.
--
-- instance Functor (More a) where
--     fmap f (L a b a') = L a (f b) a'
--     fmap f (R b a b') = R (f b) a (f b')
--
-- (INCORRECT BY CHECKING ANSWER KEY, didn't pay attention and changed the
-- Functor declaration instead of rearranging the type arguments)
--
-- (CORRECT BY CHECKING ANSWER KEY)
data More' b a = L' a b a | R' b a b deriving (Eq, Show)

instance Functor (More' x) where
    fmap f (L' a b a') = L' (f a) b (f a')
    fmap f (R' b a b') = R' b (f a) b'
