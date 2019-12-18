-- GimmePerson.hs
module GimmePerson where

import Data.Either (isRight)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String
    deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise =
        Left $ PersonInvalidUnknown $
            "Name was: " ++ show name ++
            " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStr "Give me a name: "
    name <- getLine
    putStr "Give me an age: "
    age <- getLine

    -- (INCORRECT, COMPILE-TIME ERROR)
    -- (PERSONAL NOTE: No '=' sign allowed within a do block, adding a do block
    -- afterwards does not help, adding a 'let' statement does not help)
    --
    -- personOrError = mkPerson name (read age)
    -- if (isRight personOrError)
    -- then
    --     print $ "Here's the person: " ++ personOrError
    -- else
    --     print $ "Here's the error: " ++ personOrError
    --
    -- (FROM ANSWER KEY)
    let person = mkPerson name (read age) in
        case person of
            (Left _) -> putStrLn ("Invalid person: " ++ (show person))
            (Right _) -> putStrLn ("Valid person: " ++ (show person))
