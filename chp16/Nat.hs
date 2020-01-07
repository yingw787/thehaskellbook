{-# LANGUAGE RankNTypes #-}

type Nat f g = forall a . f a -> g a

-- This will work
maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- This won't work, because `(+)` implies Num typeclass, and therefore would not
-- be agnostic to affecting only the structure itself.
--
-- degenerateMtl :: Nat Maybe []
-- degenerateMtl Nothing = []
-- degenerateMtl (Just a) = [a + 1]
