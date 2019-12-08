-- greetIfCool3.hs
module GreetIfCool3 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
    -- Matching data constructor for `Bool`, of which cool is an instance.
    case cool of
        True ->
            putStrLn "eyyyy. What's shakin'?"
        False ->
            putStrLn "pshhh."
    where cool =
        coolness == "downright frosty yo"
