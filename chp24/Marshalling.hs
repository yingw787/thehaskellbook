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
