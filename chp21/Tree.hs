-- Tree.hs
module Tree where

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap = undefined

-- foldMap is a bit easier and looks more natural, but you can do foldr too for
-- extra credit.
instance Foldable Tree where
    foldMap = undefined

instance Traversable Tree where
    traverse = undefined
