-- Marshalling.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Marshalling where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Text.RawString.QQ


sectionJson :: ByteString
sectionJson = [r|
{   "section": {"host": "wikipedia.org"},
    "whatisit": {"red": "intoothandclaw"}
}
|]


-- Error due to mixing Data.ByteString (ByteString) and Data.ByteString.Lazy
-- (ByteString)
--
-- main :: IO ()
-- main = do
--     let blah :: Maybe Value
--         blah = decodeStrict sectionJson

--     print blah
