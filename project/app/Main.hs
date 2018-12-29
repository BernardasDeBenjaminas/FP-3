{-# LANGUAGE OverloadedStrings #-}

module Main where

import Game
import Data.Monoid (mconcat)
import Web.Scotty

import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as L    

main = scotty 3000 $ do

    post "/:word" $ do
        bd <- body
        case getGameMsg $ L.unpack $ decodeUtf8 bd of
            Left  e0 -> html $ mconcat ["Rec: ", decodeUtf8 bd, "\n", "Resp: ", L.pack e0]
            Right resp -> html $ mconcat [L.pack resp]

    get "/:word" $ do
        html "test"