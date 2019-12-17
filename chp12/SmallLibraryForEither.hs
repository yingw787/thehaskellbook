-- SmallLibraryForEither.hs
module SmallLibraryForEither where

-- 1)
--
-- (CORRECT BY GHCI OUTPUT, MATCHES ANSWER KEY)
lefts' :: [Either a b] -> [a]
lefts' x = foldr f' [] x where
    f' (Left x') y = (x' : y)
    f' _ y = y

-- 2)
--
-- (CORRECT BY GHCI OUTPUT, MATCHES ANSWER KEY)
rights' :: [Either a b] -> [b]
rights' x = foldr f' [] x where
    f' (Right x') y = (x' : y)
    f' _ y = y

-- 3)
--
-- (CORRECT BY GHCI OUTPUT)
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' x = go x [] [] where
    go [] l r = (l, r)
    go ((Left x') : xs) l r = go xs (l ++ [x']) r
    go ((Right x') : xs) l r = go xs l (r ++ [x'])
-- (ANSWER KEY HAS SIMPLER SOLUTION)
-- partitionEithers' x = (lefts' x, rights' x)

-- 4)
--
-- (ALMOST CORRECT, HAD TO CORRECT `Maybe` to `Just` but looked at answer key)
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just (f x)
eitherMaybe' f (Left x) = Nothing

-- 5)
--
-- (CORRECT BY GHCI COMPILE, MATCHES ANSWER KEY)
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left x) = f x
either' f g (Right x) = g x

-- 6)
--
-- (CORRECT BY GHCI OUTPUT)
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f (Left x) = Nothing
eitherMaybe'' f (Right x) = Just (f x)
-- (ANSWER KEY HAS DIFFERENT SOLUTION)
-- eitherMaybe'' f e = either' (const Nothing) (Just . f) e

main :: IO ()
main = do
    -- [1, 3]
    print $ lefts' [Left 1, Right 2, Left 3]

    -- [1, 3]
    print $ rights' [Right 1, Left 2, Right 3]

    -- ([1, 3], [2])
    print $ partitionEithers' [Left 1, Right 2, Left 3]
