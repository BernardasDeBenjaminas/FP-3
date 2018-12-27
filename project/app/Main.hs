{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Monoid (mconcat)
import Web.Scotty

import Data.Text.Lazy.Encoding (decodeUtf8)

main = scotty 3000 $ do

    post "/:word" $ do
        beam <- param "word"
        bd <- body
        html $ mconcat ["<h1>Scotty, ", beam, decodeUtf8 bd, " me up!</h1>"]

    get "/:word" $ do
        html "test"