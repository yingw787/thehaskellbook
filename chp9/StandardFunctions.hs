-- StandardFunctions.hs
module StandardFunctions where

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
myAnd' (x : xs) = x && myAnd xs

-- 1)
myOr :: [Bool] -> Bool
-- If there are no matches, then return False by default.
myOr [] = False
myOr (x : xs) =
    if x == True
    then True
    else myOr xs

myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x : xs) = x || myOr' xs

-- 2)
myAny :: (a -> Bool) -> [a] -> Bool
myAny f a = myOr [f x | x <- a]

-- 3)
myElem :: Eq a => a -> [a] -> Bool
myElem a b = myAny (\x -> x == a) b

-- 4)
--
-- (PERSONAL NOTE: While this answer is technically correct, there may be
-- performance penalties creating a new list every single time. Consider using a
-- `go` method in order to inform compiler that the same list can/should be
-- used.)
--
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ x :[]

-- 5)
--
-- (CORRECT, GHCI RESULTS BELOW)
--
-- *StandardFunctions> squish [[1], [2], [3]]
-- [1,2,3]
-- *StandardFunctions>
--
squish :: [[a]] -> [a]
squish [] = []
squish (x : xs) = x ++ squish xs

-- 6)
--
-- (CORRECT-ISH, ANSWER KEY https://github.com/johnchandlerburnham/hpfp)
-- (PERSONAL NOTE: I don't really understand the answer key...so here's the
-- original solution.)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x : xs) = (f x) ++ (squishMap f xs)

-- 7)
--
-- (CORRECT, BY GHCI OUTPUT)
--
squishAgain :: [[a]] -> [a]
squishAgain a = squishMap id a

-- 8)
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
-- (PERSONAL NOTE: I'm confused for the case where there is an empty list. How
-- is that handled? I don't think it's `f _ [] = []`.)
--
-- (INCORRECT, ANSWER KEY BELOW: https://github.com/johnchandlerburnham/hpfp)
--
-- (PERSONAL NOTE: Answer key solution also fails on empty list with
-- non-exhaustive patterns.)
myMaximumBy cmp (x:xs) = go cmp xs x
    where
        -- You can have multiple `go` functions within the where block.
        -- (PERSONAL NOTE: I didn't know that.)
        go _ [] a = a
        go cmp (x:xs) a =
            go cmp xs (if (cmp x a) == GT then x else a)

-- 9)
--
-- (CORRECT BY GHCI)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp (x : xs) = go cmp xs x
    where
        go _ [] a = a
        go cmp (x : xs) a =
            go cmp xs (if (cmp x a) == LT then x else a)

-- 10)
--
-- (CORRECT BY GHCI)
myMaximum :: (Ord a) => [a] -> a
myMaximum a = myMaximumBy compare a

myMinimum :: (Ord a) => [a] -> a
myMinimum a = myMinimumBy compare a
