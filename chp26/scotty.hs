-- scotty.hs
{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Control.Monad.Trans.Class
import Web.Scotty

import Data.Monoid (mconcat)


main = scotty 3000 $ do
    get "/:word" $ do
        beam <- param "word"
        lift (putStrLn "hello")

        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
