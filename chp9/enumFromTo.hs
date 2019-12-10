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

-- Generalizing based on ANSWER KEY: https://github.com/johnchandlerburnham/hpfp

-- Typeclass constraints that type must be order-able, and enumerable.
eft :: (Ord a, Enum a) => a -> a -> [a]
eft x y = go x y []
    where
        go a b c
            | a > b = c
            -- | a == b = reverse (a : c)
            --
            -- The operand `reverse` is weird, this is replacement.
            | a == b = c ++ a:[]
            | otherwise = go (succ a) b (a : c)

-- Use recursion and avoid having to use `go` functions.
--
-- Method `eft3`
eft3 :: (Ord a, Enum a) => a -> a -> [a]
eft3 x y
    | x > y = []
    | x == y = [x]
    -- The recursion works because the ending constructor is of type list, which
    -- successfully applies the `cons` constructor.
    -- 1 : []
    -- [1]
    -- 1 : 2 : []
    -- [1,2]
    -- 1 : 2 : [3]
    -- [1,2,3]
    | otherwise = x : eft3 (succ x) y

-- We can get rid of the `Ord` typeclass constraint by leveraging the mapping
-- between an Enum and Int using method `fromEnum`:
--
-- fromEnum True
-- 1
-- fromEnum False
-- 0
-- fromEnum 'a'
-- 97 -- Unicode decimal representation
-- fromEnum 'Ļ'
-- 315 -- Yes, it really is Unicode
eft4 :: Enum a => a -> a -> [a]
eft4 x y
    | fromEnum x > fromEnum y = []
    | fromEnum x == fromEnum y = [x]
    | otherwise = x : eft4 (succ x) y
