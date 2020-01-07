-- HeavyLifting.hs
--
-- Some of these examples will not compile unless they are modified, so they are
-- commented out.
module HeavyLifting where

-- 1)
-- a = (+1) $ read "[1]" :: [Int]
fmap (+1) $ read "[1]" :: [Int]

-- 2)
-- b = (++ "lol") (Just ["Hi,", "Hello"])
(fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3)
-- c = (*2) (\x -> x - 2) 1
fmap (*2) (\x -> x - 2) 1

-- 4)
-- d = ((return '1' ++) . show) (\x -> [x, 1..3])
fmap ((return '1' ++) . show) (\x -> [x, 1..3]) 0

-- 5)
-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123"++) show ioi
--     in (*3) changed
--
-- (PERSONAL NOTE: Looked at answer key but when I plugged it in it gave an
-- error, so I ended up working this out myself anyways)
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read $ fmap ("123"++) $ fmap show ioi
    in fmap (*3) changed
