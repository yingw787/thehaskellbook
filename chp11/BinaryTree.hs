-- BinaryTree.hs
module BinaryTree where

-- Constructor for a binary tree
data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

  -- Inserting into a binary tree
insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a = Node (insert' b left) a right
    | b > a = Node left a (insert' b right)

-- Create `map()` for binary tree.
mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
-- SOLUTION BELOW
--
-- (CORRECT BY GHCI OUTPUT)
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected =
    Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

-- acceptance test for mapTree
mapOkay =
    if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

-- main :: IO ()
-- main = do
--     mapOkay

-- Convert binary trees to lists
preorder :: BinaryTree a -> [a]
-- (INCORRECT, COMPILE-TIME ERROR)
-- preorder _ Leaf = []
-- preorder (Node Leaf a Leaf) = a
-- preorder (Node left a right) = Node (preorder left) a right
--
-- (PERSONAL NOTE: not sure where I was going with this... `cons` operator
-- concatenating a binary tree into a list??
--
-- preorder (Node Leaf a Leaf) = a
-- preorder (Node left a right) = (preorder (BinaryTree left)) : B
--
-- (SIGH ANSWER KEY..)
preorder Leaf = []
preorder (Node left a right) = [a] ++ (preorder left) ++ (preorder right)

-- (CORRECT BY GHCI OUTPUT)
inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = (preorder left) ++ [a] ++ (preorder right)

-- (CORRECT BY GHCI OUTPUT)
postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = (postorder left) ++ (postorder right) ++ [a]

testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

-- main :: IO ()
-- main = do
--     testPreorder
--     testInorder
--     testPostorder

-- Write `foldr` for BinaryTree
--
-- (EUGH BIG BRAIN TIME...)
-- (YEAH NOPE. ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
-- foldTree f a (Node left a right)
--
-- I'm guessing ac is accumulator and bt is binaryTree.
-- (PERSONAL NOTE: The guy didn't fold over the tree, he folds over a flattened
-- list like a regular fold.)
foldTree f ac bt = foldr f ac (inorder bt)
