-- HuttonsRazor.hs
module HuttonsRazor where

-- 1)
--
-- (CORRECT BY GHCI OUTPUT)
data Expr = Lit Integer | Add Expr Expr
eval :: Expr -> Integer
-- Pattern match on a literal to retrieve value within
eval (Lit n) = n
-- Evaluate both expressions before adding them together
eval (Add expr1 expr2) = (+) (eval expr1) (eval expr2)

-- 2)
--
-- (CORRECT BY GHCI OUTPUT)
printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add expr1 expr2) = (printExpr expr1) ++ " + " ++ (printExpr expr2)

a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2

main :: IO ()
main = do
    print $ eval (Add (Lit 1) (Lit 9001))
    print $ printExpr (Add (Lit 1) (Lit 9001))
    print $ printExpr a3
