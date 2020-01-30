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
import Network.Socket.ByteSTring (recv, sendAll)
import Text.RawString.QQ
