-- OtherThanList.hs
module OtherThanList where

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

-- 1)
-- unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
-- unfold = undefined
--
-- (NOT SURE WHERE TO START)
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f s = case f s of
    Nothing -> Leaf
    Just (l, v, r) -> Node (unfold f l) v (unfold f r)

-- 2)
treeBuild :: Integer -> BinaryTree Integer
-- (Not sure how to get the root value to be zero...not sure how to strip down
-- to base case condition while also ensuring that recursion converges.)
--
-- treeBuild 0 = Leaf
-- treeBuild x = Node (treeBuild (x - 1)) (x - 1) (treeBuild (x - 1))
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
-- (NEEDED TO COMPLETE THE FIRST PART OF THE QUESTION FIRST)
treeBuild n = unfold (f n) 0 where
    f n x = if x >= n then Nothing else Just (x + 1, x, x + 1)
