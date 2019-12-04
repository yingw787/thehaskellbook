-- bad_example.hs
module BadExample where

-- Class definition of `Numberish` dosen't define any terms or code we can
-- compile and execute, only types. Code lives in instances of `Age` and `Year`.
class Numberish a where
    fromNumber :: Integer -> a
    toNumber :: a -> Integer

-- pretend newtype is data for now
newtype Age =
    Age Integer
    deriving (Eq, Show)

instance Numberish Age where
    fromNumber n = Age n
    toNumber (Age n) = n

newtype Year =
    Year Integer
    deriving (Eq, Show)

instance Numberish Year where
    fromNumber n = Year n
    toNumber (Year n) = n

-- After the first parameter is provided to method `sumNumberish`, concrete
-- types are resolved by the input arguments to `sumNumberish` only.
sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
    where integerOfA = toNumber a
          integerOfAPrime = toNumber a'
          summed =
            integerOfA + integerOfAPrime
