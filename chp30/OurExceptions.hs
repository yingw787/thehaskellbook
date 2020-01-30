-- OurExceptions.hs
module OurExceptions where

import Control.Exception


data NotDivThree = NotDivThree deriving (Eq, Show)
instance Exception NotDivThree

data NotEven = NotEven deriving (Eq, Show)
instance Exception NotEven

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
    | rem i 3 /= 0 = throwIO NotDivThree
    | odd i = throwIO NotEven
    | otherwise = return i
