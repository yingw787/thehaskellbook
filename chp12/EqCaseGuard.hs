-- EqCaseGuard.hs
module EqCaseGuard where

-- If you don't want to derive `Show`, you can implement it with this:
--
-- Compiles without Eq
--
-- toString :: PersonInvalid -> String
-- toString NameEmpty = "NameEmpty"
-- toString AgeTooLow = "AgeTooLow"
-- instance Show PersonInvalid where
--     show = toString

-- Does not work without an Eq instance
--
-- blah :: PersonInvalid -> String
-- blah pi
--     | pi == NameEmpty = "NameEmpty"
--     | pi == AgeTooLow = "AgeTooLow"
--     | otherwise = "???"

-------------------------------------------------------------------------------

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

-- Important to derive `Eq` to equality check constructors.
--
-- See method `blah` for more details.
data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

-- Method `mkPerson` enables us to create a valid Person instance.
--
-- However, with this guard syntax, we can only express one error at a time.
--
mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age >= 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | otherwise = Left AgeTooLow

type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
    True -> Right age
    False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
    True -> Right name
    False -> Left [NameEmpty]

-- Method `mkPerson2` iterates on method `mkPerson` by returning a list of
-- all errors over the data.
mkPerson2 :: Name -> Age -> ValidatePerson Person
mkPerson2 name age =
    mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge
