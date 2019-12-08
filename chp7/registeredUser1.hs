-- registeredUser1.hs
module RegisteredUser where

newtype Username =
    Username String

newtype AccountNumber =
    AccountNumber Integer

-- Sum of two constructors, `UnregisteredUser` and `RegisteredUser`. Pattern
-- matching enables dispatching based on input arguments. If `RegisteredUser`,
-- then it is a product of two `newtypes`s, `Username` and `AccountNumber`.
data User =
    UnregisteredUser
    | RegisteredUser Username AccountNumber
