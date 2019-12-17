-- SmallLibraryForMaybe.hs
module SmallLibraryForMaybe where

-- 1)
--
-- (CORRECT BY GHCI OUTPUT)
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just x) = True

-- (CORRECT BY GHCI OUTPUT)
isNothing :: Maybe a -> Bool
isNothing x = not $ isJust x

-- 2)
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee a _ Nothing = a
-- Not sure how to define the latter case...
--
-- Cannot assume that you can use (+) because the typeclass constraints are not
-- present. Not sure how to combine values a and b.
--
-- I think I don't need to worry about a and b not being the same type because
-- that is handled by the typechecker.
--
-- mayybee a f (Just b) = a + f b
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
-- (PERSONAL NOTE: I should review what a catamorphism is; a is just ignored in
-- the happy path case.)
mayybee a f (Just b) = f b

-- 3)
fromMaybe :: a -> Maybe a -> a
--
-- (Not sure what I'm doing here)
--
-- fromMaybe a b = mayybee a (+0) b
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
-- (PERSONAL NOTE: I did consider using `id`, but didn't because it seemed..too
-- obvious)
fromMaybe a b = mayybee a (id) b

-- 4)
--
-- (CORRECT BY GHCI OUTPUT)
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

-- (CORRECT BY GHCI OUTPUT)
maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5)
--
-- (CORRECT BY GHCI OUTPUT)
catMaybes :: [Maybe a] -> [a]
catMaybes a = map fromMaybe' $ filter isJust a where
    fromMaybe' (Just x) = x

-- 6)
--
-- (CORRECT BY GHCI OUTPUT)
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe a = go a [] where
    go (Nothing : _) _ = Nothing
    go [] x = Just x
    go ((Just x) : xs) xss = go xs (xss ++ [x])
