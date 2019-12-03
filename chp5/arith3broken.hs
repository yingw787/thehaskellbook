-- arith3broken.hs

-- Original problems:
-- - first print statement should wrap `1 + 2` in parentheses, or use `$`
--   operator to execute right-side of operation first.
-- - second print statement may need to cast numeric type to string type for
--   method `putStrLn`.
-- - `-1` should be wrapped in parentheses in order to apply sectioning
--   correctly.
-- - `where` clause should be indented.

module Arith3Broken where

main :: IO ()
Main = do
    print 1 + 2
    putStrLn 10
    print (negate -1)
    print ((+) 0 blah)
    where blah = negate 1
