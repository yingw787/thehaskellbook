-- WarmingUp.hs
module WarmingUp where

import Data.Char


cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs


composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev


tupled :: [Char] -> ([Char], [Char])
tupled xs = (cap xs, rev xs)

-- (PERSONAL NOTE: Not sure how to use the Applicative context here, as per book
-- instructions)
--
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> cap <*> rev

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
tupled'' :: [Char] -> ([Char], [Char])
tupled'' = do
    c <- cap
    r <- rev
    return (c, r)

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
tupleBind :: [Char] -> ([Char], [Char])
tupleBind str = cap <$> rev >>= (,) $ str

-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
tupleBind' :: [Char] -> ([Char], [Char])
tupleBind' = rev >>= (\x -> cap >>= \y -> return (x, y))


main :: IO ()
main = do
    print $ composed "Julie"
    print $ composed "Chris"

    print $ fmapped "Julie"
    print $ fmapped "Chris"

    print $ tupled "Julie"

    print $ tupled' "Julie"

    print $ tupled'' "Julie"

    print $ tupleBind "Julie"

    print $ tupleBind' "Julie"
