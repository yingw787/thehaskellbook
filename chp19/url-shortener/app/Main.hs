{-# LANGUAGE OverloadedStrings #-}

-- OverloadedStrings is a GHC language extension that makes type 'String'
-- polymorphic (abstract types 'Text' and 'ByteString') by default.
--
-- Prelude Data.String> :t "blah"
-- "blah" :: [Char]
-- Prelude Data.String> :set -XOverloadedStrings
-- Prelude Data.String> :t "blah"
-- "blah" :: IsString p => p
-- Prelude Data.String> import Data.Text (Text)
-- Prelude Data.String Data.Text> import Data.ByteString (ByteString)
-- Prelude Data.String Data.Text Data.ByteString> t = "blah" :: Text
-- Prelude Data.String Data.Text Data.ByteString> bs = "blah" :: ByteString
-- Prelude Data.String Data.Text Data.ByteString> t == bs

-- <interactive>:12:6: error:
--     • Couldn't match expected type ‘Text’ with actual type ‘ByteString’
--     • In the second argument of ‘(==)’, namely ‘bs’
--       In the expression: t == bs
--       In an equation for ‘it’: it = t == bs
-- Prelude Data.String Data.Text Data.ByteString>

-- Module name must be "Main" required for anything exporting a "main"
-- executable.
module Main where

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import Network.URI (URI, parseURI)
import qualified System.Random as SR
import Web.Scotty


alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  -- Right of arrow is IO Int,
  -- so randomDigit is Int
  randomDigit <- SR.randomRIO (0, maxIndex)
  return (xs !! randomDigit)

shortyGen :: IO [Char]
shortyGen =
  replicateM 7 (randomElement alphaNum)

saveURI :: R.Connection
            -> BC.ByteString
            -> BC.ByteString
            -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

getURI :: R.Connection
            -> BC.ByteString
            -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI =
  R.runRedis conn $ R.get shortURI

linkShorty :: String -> String
linkShorty shorty =
  concat
  [ "<a href=\""
  , shorty
  , "\">Copy and paste your short URL</a>"
  ]

-- TL.concat :: [TL.Text] -> TL.Text
shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            , " shorty is: "
            , TL.pack (linkShorty shawty)
            ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat
    [ uri
    , " wasn't a url,"
    , " did you forget http://?"
    ]

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat
    [ "<a href/\""
    , tbs, "\">"
    , tbs, "</a>" ]

app :: R.Connection -> ScottyM ()
app rConn = do
  get "/" $ do
    uri <- param "uri"
    let parsedUri :: Maybe URI
        parsedUri = parseURI (TL.unpack uri)

    case parsedUri of
      Just _ -> do
        shawty <- liftIO shortyGen
        let shorty = BC.pack shawty
            uri' =
              encodeUtf8 (TL.toStrict uri)
        resp <-
          liftIO (saveURI rConn shorty uri')
        html (shortyCreated resp shawty)
      Nothing -> text (shortyAintUri uri)

  get "/:short" $ do
    short <- param "short"
    uri <- liftIO (getURI rConn short)

    case uri of
      Left reply ->
        text (TL.pack (show reply))
      Right mbBS -> case mbBS of
        Nothing -> text "uri not found"
        Just bs -> html (shortyFound tbs) where
          tbs :: TL.Text
          tbs = TL.fromStrict (decodeUtf8 bs)



main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  scotty 3000 (app rConn)
