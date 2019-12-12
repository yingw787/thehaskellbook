-- DatabaseProcessing.hs
module DatabaseProcessing where

import Data.Time

data DatabaseItem   = DbString String
                    | DbNumber Integer
                    | DbDate UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
                (fromGregorian 1911 5 1)
      (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
                (fromGregorian 1921 5 1)
                (secondsToDiffTime 34123))
    ]

-- 1)
--
-- (LITERALLY NO IDEA HOW TO GET STARTED, BLANKING OUT)
--
-- (ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
--
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = map unpack . filter isADbDate where
    isADbDate (DbDate _) = True
    isADbDate _ = False
    unpack (DbDate utc) = utc

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' = foldr unpackDate [] where
    unpackDate (DbDate utc) acc = utc : acc
    unpackDate _ acc = acc

-- 2)
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = undefined

-- 3)
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = undefined

-- 4)
sumDb :: [DatabaseItem] -> Integer
sumDb = undefined

-- 5)
avgDb :: [DatabaseItem] -> Double
avgDb = undefined

main :: IO ()
main = do
    -- 1)
    print $ filterDbDate theDatabase
    print $ filterDbDate' theDatabase
