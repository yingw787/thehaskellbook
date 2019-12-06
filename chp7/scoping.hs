-- scoping.hs
module Scoping where

addOne :: Integer -> Integer
addOne x = x + 1

-- bindExp :: Integer -> String
-- bindExp x =
--     -- y is in scope because let expression binds y to 5. y is only in scope
--     -- inside the let expression.
--     let y = 5 in
--         "the integer was: " ++ show x
--         ++ " and y was: " ++ show y

-- The following method doesn't work:
-- error: Variable not in scope: y :: Integer
--
-- y is bound in the expression z wraps, so it is not available in let
-- expression that defines z.

-- bindExp :: Integer -> String
-- bindExp x =
--     let z = y + x in
--         let y = 5 in
--             "the integer was: "
--             ++ show x ++ " and y was: "
--             ++ show y ++ " and z was: "
--             ++ show z

-- Sometimes, function arguments aren't visible in the method if they're
-- **shadowed**. Shadowing is effectively scope overwriting.
--
-- The following method can be applied to any integer and the result will be the
-- same, because the input argument x is shadowd by the x from the let binding.
--
-- Haskell follows lexical scoping: resolving value for named entity depends on
-- location in code and lexical context.

-- bindExp :: Integer -> String
-- bindExp x =
--     let x = 10; y = 5 in
--         "the integer was: " ++ show x
--         ++ " and y was: " ++ show y

main :: IO ()
main = do
    print $ addOne 1
    print $ bindExp 8
