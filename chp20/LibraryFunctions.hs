-- LibraryFunctions.hs
module LibraryFunctions where

import Data.Foldable
import Data.Monoid


-- 1)
sum' :: (Foldable t, Num a) => t a -> a
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
sum' xs = foldr (+) 0 xs

-- 2)

-- 3)

-- 4)

-- 5)

-- 6)

-- 7)

-- 8)

-- 9)

-- 10)


main :: IO ()
main = do
    print $ sum' [1..5]
