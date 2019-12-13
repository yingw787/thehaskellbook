-- RewritingFunctions.hs
module RewritingFunctions where

-- direct recursion, not using (&&)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x : xs) =
    if x == False
    then False
    else myAnd xs

-- direct recursion, using (&&)
myAnd' :: [Bool] -> Bool
myAnd' [] = True
myAnd' (x : xs) = x && myAnd' xs

-- fold, not pointfree
myAnd'' :: [Bool] -> Bool
myAnd'' = foldr (\a b -> if a == False then False else b) True

-- fold, both myAnd and the folding function are pointfree now
myAnd''' :: [Bool] -> Bool
myAnd''' = foldr (&&) True


-- 1)
myOr :: [Bool] -> Bool
myOr = undefined

-- 2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny = undefined

-- 3)
myElem :: Eq a => a -> [a] -> Bool
myElem = undefined

myElem' :: Eq a => a -> [a] -> Bool
myElem' = undefined

-- 4)
myReverse :: [a] -> [a]
myReverse = undefined

-- 5)
myMap :: (a -> b) -> [a] -> [b]
myMap = undefined

-- 6)
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = undefined

-- 7)
squish :: [[a]] -> [a]
squish = undefined

-- 8)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap = undefined

-- 9)
squishAgain :: [[a]] -> [a]
squishAgain = undefined

-- 10)
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = undefined

-- 11)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = undefined
