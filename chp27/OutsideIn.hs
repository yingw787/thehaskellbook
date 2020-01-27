-- OutsideIn.hs
--
-- Strict languages can't do this, because they force bottom before binding x.
-- Strict languages evaluate bindings as they come into scope, not when binding
-- is used.
module OutsideIn where


hypo :: IO ()
hypo = do
    let x :: Int
        x = undefined
    s <- getLine

    case s of
        "hi" -> print x
        _ -> putStrLn "hello"


-- Strict version of 'hypo'; `seq` forces evaluation of first argument if and
-- when second argument is evaluated.
hypo' :: IO ()
hypo' = do
    let x :: Integer
        x = undefined
    s <- getLine
    case x `seq` s of
        "hi" -> print x
        _ -> putStrLn "hello"


-- Moved `seq` to earliest point in IO action
-- This will result in an evaluation to bottom before evaluating getLine, which
-- will raise an exception.
hypo'' :: IO ()
hypo'' = do
    let x :: Integer
        x = undefined
    s <- seq x getLine

    case s of
        "hi" -> print x
        _ -> putStrLn "hello"
