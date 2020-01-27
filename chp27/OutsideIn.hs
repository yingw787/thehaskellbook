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
