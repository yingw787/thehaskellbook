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
-- (FROM ANSWER KEY: https://github.com/johnchandlerburnham/hpfp)
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
--
-- (CORRECT)
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = map unpack . filter isADbNumber where
    isADbNumber (DbNumber _) = True
    isADbNumber _ = False
    unpack (DbNumber number) = number

filterDbNumber' :: [DatabaseItem] -> [Integer]
filterDbNumber' = foldr unpackNumber [] where
    unpackNumber (DbNumber number) acc = number : acc
    unpackNumber _ acc = acc

-- 3)
--
-- (LOST HERE,  NOT SURE WHAT THE IDENTITY VALUE FOR A UTCTIME datatype is
-- supposed to be)
--
-- (FROM ANSWER KEY)
mostRecent :: [DatabaseItem] -> UTCTime
-- mostRecent = foldr maxDate
mostRecent db = foldr compareDate base db where
    compareDate (DbDate utc) acc = max utc acc
    compareDate _ acc = acc
    -- Assume that there is at least one element in the list of DatabaseItems
    -- that is of type UTCTime.
    base = (filterDbDate' db) !! 0

-- 4)
--
-- (CORRECT BY ANSWER KEY)
sumDb :: [DatabaseItem] -> Integer
sumDb db = foldr addDbNumbers 0 db where
    addDbNumbers (DbNumber num) acc = num + acc
    addDbNumbers _ acc = acc

-- 5)
--
-- (INCORRECT)
avgDb :: [DatabaseItem] -> Double
-- avgDb db = (fst / snd) $ foldr avgNumber (0, 0) db where
--     avgNumber (DbNumber num) (sum, numElems) = (num + sum, numElems + 1)
--     avgNumber _ acc = acc
--
-- (FROM ANSWER KEY)
--
-- (I missed wrapping the method in a where clause.)
avgDb db = (fromIntegral $ fst tup) / (fromIntegral $ snd tup) where
    tup = foldr addNum (0, 0) db
    addNum (DbNumber int) (num, den) = (int + num, den + 1)
    addNum _ acc = acc

avgDb' :: [DatabaseItem] -> Double
avgDb' db = n / d where
    n = (fromIntegral $ sumDb db)
    d = (fromIntegral $ length $ filterDbNumber db)

main :: IO ()
main = do
    -- 1)
    print $ filterDbDate theDatabase
    print $ filterDbDate' theDatabase

    -- 2)
    print $ filterDbNumber theDatabase
    print $ filterDbNumber' theDatabase

    -- 3)
    print $ mostRecent theDatabase

    -- 4)
    print $ sumDb theDatabase

    -- 5)
    print $ avgDb theDatabase
    print $ avgDb' theDatabase
