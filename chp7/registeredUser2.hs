-- registeredUser2.hs
module RegisteredUser where

newtype Username =
    Username String

newtype AccountNumber =
    AccountNumber Integer

data User =
    UnregisteredUser
    | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser =
    putStrLn "UnregisteredUser"

-- Pattern matching for method on RegisteredUser constructor.
printUser (RegisteredUser
    (Username name)
    (AccountNumber acctNum)) =
        putStrLn $ name ++ " " ++ show acctNum
