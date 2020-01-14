# Chapter 19

- Applying structure

- Templating content in `scotty`
    - https://github.com/scotty-web/scotty

- Concatenating connection parameters
    - http://adit.io/posts/2013-04-15-making-a-website-with-haskell.html

- Concatenating key configurations
    - Monoid of functions

```haskell
Prelude> import Data.Monoid
Prelude Data.Monoid> f = const (Sum 1)
Prelude Data.Monoid> g = const (Sum 2)
Prelude Data.Monoid> f 9001
Sum {getSum = 1}
Prelude Data.Monoid> g 9002
Sum {getSum = 2}
-- (PERSONAL NOTE: I didn't get the book definition of the Monoid of functions,
-- defined as `instance Monoid b => Monoid (a -> b)`.)
Prelude Data.Monoid> :t f
f :: Num a => b -> Sum a
Prelude Data.Monoid> :t g
g :: Num a => b -> Sum a
Prelude Data.Monoid> (f <> g) 9001
Sum {getSum = 3}
Prelude Data.Monoid>
```

```haskell
Prelude> import qualified Data.Map as M
Prelude M> :t M.fromList
M.fromList :: Ord k => [(k, a)] -> M.Map k a
Prelude M> let f = M.fromList [('a', 1)]
Prelude M> let g = M.fromList [('b', 2)]
Prelude M> :t f
f :: Num a => M.Map Char a
Prelude M> import Data.Monoid
Prelude M Data.Monoid> f <> g
fromList [('a',1),('b',2)]
Prelude M Data.Monoid> :t (f <> g)
(f <> g) :: Num a => M.Map Char a
Prelude M Data.Monoid> mappend f g
fromList [('a',1),('b',2)]
Prelude M Data.Monoid> f <> g
fromList [('a',1),('b',2)]
```

- `Functor`
    - Lifting over `IO`

```haskell
import Data.Time.Clock

offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset =
    fmap (addUTCTime (offset * 24 * 3600)) $
        getCurrentTime
```

```haskell
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4

textUuid :: IO Text
textUuid =
    fmap (T.pack . UUID.toString)
        UUIDv4.nextRandom
```

- Lifting over web app monads
    - Apps are generally wrapped in monadic contexts

```haskell
userAgent :: AppHandler (Maybe UserAgent)
userAgent =
    (fmap . fmap) userAgent' getRequest

userAgent' :: Request -> Maybe UserAgent
userAgent' req =
    getHeader "User-Header" req
```

- Applicative

- `hgrev`

```haskell
jsonSwitch :: Parser (a -> a)
jsonSwitch =
    infoOption $(hgRevStateTH jsonFormat) $ long "json" <> short 'J' <> help "Display JSON version information"

parserInfo :: ParserInfo (a -> a)
parserInfo =
    -- <* is applicative sequencing operator
    info (helper <*> verSwitch <* jsonSwitch) fullDesc
```

- More parsing

```haskell
-- Parse JSON
parseJSON :: Value -> Parser a

(.:) :: FromJSON a => Object -> Text -> Parser a

instance FromJSON Payload where
    parseJSON (Object v) =
        PayLoad <$> v .: "from"
                <*> v .: "to"
                <*> v .: "subject"
                <*> v .: "body"
                <*> v .: "offset_seconds"
    parseJSON v = typeMismatch "Payload" v

-- Parse CSV
parseRecord :: Record -> Parser a

instance FromRecord Release where
    parseRecord v
        | V.length v == 5 = Release <$> v .! 0
                                    <*> v .! 1
                                    <*> v .! 2
                                    <*> v .! 3
                                    <*> v .! 4
        | otherwise = mzero

instance Deserializable ShowInfoResp where
    parser =
        e2err =<< convertPairs . HM.fromList <$> parsePairs
        where
            parsePairs :: Parser [(Text, Text)]
            parsePairs =
                parsePair `sepBy` endOfLine
            parsePair =
                liftA2 (,) parseKey parseValue
            parseKey =
                takeTill (==':') <* kvSep
            kvSep = string ": "
            parseValue = takeTill isEndOfLine
```

- And now for something different

```haskell
module Web.Shipping.Utils ((<||>)) where

import Control.Applicative (liftA2)

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) = liftA2 (||)
```

```haskell
Prelude> True || False
True
Prelude> False || False
False
Prelude> (2 > 3) || (3 == 3)
True
Prelude> import Control.Applicative
-- Define a helper infix operation to lift boolean disjunction into structure
Prelude Control.Applicative> (<||>) = liftA2 (||)
Prelude Control.Applicative> f 9001 = True; f _ = False
Prelude Control.Applicative> g 42 = True; g _ = False
Prelude Control.Applicative> :t f
f :: (Eq a, Num a) => a -> Bool
Prelude Control.Applicative> f 9001
True
Prelude Control.Applicative> f 42
False
Prelude Control.Applicative> g 9001
False
Prelude Control.Applicative> g 42
True
-- Compose the two functions and lift input arguments within structure
Prelude Control.Applicative> (\n -> f n || g n) 0
False
Prelude Control.Applicative> (\n -> f n || g n) 9001
True
Prelude Control.Applicative> :t (\n -> f n || g n)
(\n -> f n || g n) :: (Eq a, Num a) => a -> Bool
-- Use the helper infix operator defined earlier to do the same thing
Prelude Control.Applicative> (f <||> g) 0
False
Prelude Control.Applicative> (f <||> g) 9001
True
Prelude Control.Applicative>
```

- Monad

- Opening a network socket

```haskell
import Network.Socket

openSocket :: FilePath -> IO Socket
openSocket p = do
    sock <- socket  AF_UNIX
                    Stream
                    defaultProtocol
    connect sock sockAddr
    return sock
    where sockAddr =
            SockAddrUnix . encodeString $ p
```

- Binding over failure in initialization

```haskell
main :: IO ()
main = do
    initAndFp <- runEitherT $ do
        fp <- tryHead NoConfig =<< lift getArgs
        initCfg <- load' fp
        return (initCfg, fp)

    either bail (uncurry boot) initAndFp
    where
        boot initCfg fp =
            void $  runMVC mempty
                    oracleModel (core initCfg fp)

        bail NoConfig =
            errorExit "Please pass a config"

        bail (InvalidConfig e) =
            errorExit
                ("Invalid config " ++ show e)

        load' fp =
            hoistEither . fmapL InvalidConfig =<< lift (load fp)
```

- URL shortener
