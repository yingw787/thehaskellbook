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
