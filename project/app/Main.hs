{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Monoid (mconcat)
import Web.Scotty

import Data.Text.Lazy.Encoding (decodeUtf8)

main = scotty 3000 $
    post "/:word" $ do
        beam <- param "word"
        bd <- body
        html $ mconcat ["<h1>Scotty, ", beam, decodeUtf8 bd, " me up!</h1>"]