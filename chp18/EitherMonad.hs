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
