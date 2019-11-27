-- greetIfCool2.hs
--
-- Identical to greetIfCool1.hs, except that cool is now a method definition
-- accepting parameter coolness.
module GreetIfCool2 where

greetIfCool :: String -> IO ()
greetIfCool coolness =
    if cool coolness
        then putStrLn "eyyyyy. What's shakin'?"
    else
        putStrLn "pshhh."
    where cool v =
            v == "downright frosty yo"
