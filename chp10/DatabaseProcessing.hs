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
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate a = undefined

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
