-- enumFromTo.hs
module EnumFromTo where

-- I have no idea where to start. ANSWER KEY
-- https://github.com/johnchandlerburnham/hpfp
--
-- Probably should use some form of recursion.
--
-- This method `eftBool` didn't really make much sense to me, since there are
-- only two concrete values of type `Bool` in the first place.
eftBool :: Bool -> Bool -> [Bool]
eftBool x y = go x y []
    where
        -- Use a `go` method to keep track of an empty list.
        go a b current
            -- Base case condition where ordering is violated, in which case
            -- return `current`.
            | a > b = current
            -- NOTE: I'm not sure why `reverse` should be called here. I think
            -- the end goal is to have just one copy of the data?
            | a == b = reverse (a : current)
            -- Apply recursion and append current value to list.
            | otherwise = go (succ a) b (a : current)

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd x y = go x y []
    where
        go first second currentList
            | first > second = currentList
            | first == second = reverse (first : currentList)
            | otherwise = go (succ first) second (first : currentList)

eftInt :: Int -> Int -> [Int]
eftInt x y = go x y []
    where
        go first second currentList
            | first > second = currentList
            | first == second = reverse (first : currentList)
            | otherwise = go (succ first) second (first : currentList)

eftChar :: Char -> Char -> [Char]
eftChar x y = go x y []
    where
        go first second currentList
            | first > second = currentList
            | first == second = reverse (first : currentList)
            | otherwise = go (succ first) second (first : currentList)
