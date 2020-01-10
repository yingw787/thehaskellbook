-- EitherMonad.hs
--
-- m ~ Either e
--
-- (>>=) :: Monad m => m a -> (a ->        m b) ->        m b
-- (>>=) ::     Either e a -> (a -> Either e b) -> Either e b
--
-- return :: Monad m => a ->        m a
-- return ::            a -> Either e a
module EitherMonad where


-- years ago
type Founded = Int

-- number of programmers
type Coders = Int

data SoftwareShop =
    Shop {
        founded :: Founded
        , programmers :: Coders
    } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
    | TooManyYears Founded
    | NegativeCoders Coders
    | TooManyCoders Coders
    | TooManyCodersForYears Founded Coders
    deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
    | n < 0 = Left $ NegativeYears n
    | n > 500 = Left $ TooManyYears n
    | otherwise = Right n

-- Tho, many programmers *are* negative.
validateCoders :: Int -> Either FoundedError Coders
validateCoders n
    | n < 0 = Left $ NegativeCoders n
    | n > 5000 = Left $ TooManyCoders n
    | otherwise = Right n

-- There is no `Monad` equivalent for `Validation` (where all errors are scraped
-- and returned to the user, like that of `Data.Validation` as `Applicative`.)
mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
    founded <- validateFounded years
    programmers <- validateCoders coders

    if programmers > div founded 10
        then Left $ TooManyCodersForYears founded programmers
        else Right $ Shop founded programmers
