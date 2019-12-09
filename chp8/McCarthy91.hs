-- McCarthy91.hs
--
-- (CORRECT, GHCI RESULTS BELOW)
--
-- Prelude> :l McCarthy91.hs
-- [1 of 1] Compiling McCarthy91       ( McCarthy91.hs, interpreted )
-- Ok, one module loaded.
-- *McCarthy91> main
-- [91,91,91,91,91,91,91,92,93,94,95,96,97,98,99,100]
-- *McCarthy91>

module McCarthy91 where

mc91 :: Integral a => a -> a
mc91 n
    | n > 100 = n - 10
    | otherwise = mc91 . mc91 $ n + 11

main :: IO ()
main = do
    print $ map mc91 [95..110]
