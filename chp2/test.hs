-- Hello world method --
sayHello :: String -> IO ()
sayHello x =
    putStrLn ("Hello, " ++ x ++ "!")


-- Triple a number --
triple x = x * 3
