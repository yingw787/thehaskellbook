-- Main.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Control.Monad (forever)
import Data.List (intersperse)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Data.Typeable
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite

import Database.SQLite.Simple.Types
import Network.Socket hiding (close, recv)
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ


data User =
  User {
    userId :: Integer
  , username :: Text
  , shell :: Text

  , homeDirectory :: Text
  , realName :: Text
  , phone :: Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User  <$> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) = toRow (id_, username, shell, homeDir, realName, phone)

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  ( id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT UNIQUE,
    shell TEXT, homeDirectory TEXT,
    realName TEXT, phone TEXT
  )
|]

insertUser :: Query
insertUser =
  "INSERT INTO users\
  \ VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * from users"

getUserQuery :: Query
getUserQuery = "SELECT * from users where username = ?"
