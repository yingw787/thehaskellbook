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
--
-- (CORRECT BY GHCI OUTPUT)
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2)
--
-- (INCORRECT, COMPILE-TIME ERROR)
myAny :: (a -> Bool) -> [a] -> Bool
-- myAny f a = foldr f False a
--
-- (PERSONAL NOTE: This is where my brain is starting to melt)
myAny f x = foldl check False x where
    check x y = x || f y

-- 3)
--
-- (CORRECT BY GHCI OUTPUT)
myElem :: Eq a => a -> [a] -> Bool
myElem a b = myAny (\x -> x == a) b

-- (I basically just copied method `myAny` and hardcoded the lambda in place of
-- the function.)
myElem' :: Eq a => a -> [a] -> Bool
myElem' a b = foldl check False b where
    check x y = x || (\x -> x == a) y
-- ANSWER KEY BELOW: https://github.com/johnchandlerburnham/hpfp
-- myElem x xs = myAny ((==) x) xs

-- 4)
myReverse :: [a] -> [a]
-- myReverse (x : xs) = foldl (++ x : []) xs
-- myReverse (x : xs) = foldl (++ (x : [])) xs
-- myReverse (x : xs) = foldl (++ (x : [])) "" xs

-- myReverse (x : xs) = foldl concat "" xs where
--     concat x y = y ++ x : []

-- myReverse (x : xs) = foldl concat "" xs where
--     concat x y = y : x

-- ANSWER KEY: https://github.com/johnchandlerburnham/hpfp
myReverse = foldl (flip (:)) []

-- 5)
--
-- (I know from `fold` that the identity value can change with the folding
-- function, it doesn't seem right to set a specific default...)
myMap :: (a -> b) -> [a] -> [b]
-- myMap f a = foldr f a
--
-- ANSWER KEY: https://github.com/johnchandlerburnham/hpfp
-- (Yes right, the default for a fold function is `[]`, the default for a method
-- that reduces is the one that varies...)
myMap f xs = foldr ((:) . f) [] xs

-- 6)
--
-- (I kinda forgot whether folding functions are supposed to reduce, I think
-- they can if you apply the cons operator to only the elements that match the
-- predicate)
myFilter :: (a -> Bool) -> [a] -> [a]
-- BLANKING DONT KNOW WHY
-- ANSWER KEY: https://github.com/johnchandlerburnham/hpfp
myFilter f xs = foldr g [] xs where
    g x y = if (f x) then (x : y) else y

-- 7)
--
-- (CORRECT BY GHCI OUTPUT)
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8)
--
-- (CORRECT BY GHCI OUTPUT)
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = foldr ((++) . f) [] xs

-- 9)
squishAgain :: [[a]] -> [a]
-- squishAgain = squishMap (++)
-- squishAgain xs = squishMap (++) xs
-- squishAgain = squishMap . (++)
--
-- (CORRECT BY GHCI OUTPUT)
squishAgain xs = squishMap (++ []) xs

-- 10)
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
--
-- myMaximumBy cmp xs = foldr g [] xs where
--     g x y = if cmp x y == GT then x else y
--
-- myMaximumBy cmp xs = foldr g LT xs where
--     g x y = if cmp x y == GT then x else y
--
-- myMaximumBy cmp xs = foldr g 0 xs where
--     g x y = if cmp x y == GT then x else y
--
-- (COMPILES BUT IS INCORRECT)
-- (Avoid typechecking error by pattern matching on first element, max of empty
-- list should raise exception anyways)
--
-- myMaximumBy cmp (x : xs) = foldr g x xs where
--     g x y = if cmp x y == GT then x else y
--
-- ANSWER KEY: https://github.com/johnchandlerburnham/hpfp
-- (OK I forgot about method `foldr1`.)
--
myMaximumBy ord xs = foldr1 g xs
    where g x y = if (ord x y) == GT then x else y

-- 11)
--
-- (CORRECT BY GHCI OUTPUT)
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy ord xs = foldr1 g xs
    where g x y = if (ord x y) == LT then x else y


main :: IO ()
main = do
    -- 1)
    print $ myOr [True, False, True]
    print $ myOr [False]

    -- 2)
    print $ myAny even [1, 3, 5]
    print $ myAny odd [1, 3, 5]

    -- 3)
    print $ myElem 1 [1..10]
    print $ myElem 1 [2..10]

    print $ myElem' 1 [1..10]
    print $ myElem' 1 [2..10]

    -- 4)
    print $ myReverse "blah"
    print $ myReverse [1..5]

    -- 5)
    print $ myMap length (words "Mary had a little lamb")

    -- 6)
    print $ myFilter even [1..10]
    print $ myFilter odd [1..10]

    -- 7)
    print $ squish [[1], [2], [3]]

    -- 8)
    print $ squishMap (\x -> [1, x, 3]) [2]
    print $ squishMap (\x -> "WO " ++ [x] ++ " OT ") "blah"

    -- 9)
    print $ squishAgain [[1], [2], [3]]

    -- 10)
    print $ myMaximumBy (\_ _ -> GT) [1..10]
    print $ myMaximumBy (\_ _ -> LT) [1..10]
    print $ myMaximumBy compare [1..10]

    -- 11)
    print $ myMinimumBy (\_ _ -> GT) [1..10]
    print $ myMinimumBy (\_ _ -> LT) [1..10]
    print $ myMinimumBy compare [1..10]
