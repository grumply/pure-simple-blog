module Main where

import Server
import Server.Config

import Shared

import Pure.Conjurer
import Pure.Elm.Component
import Pure.WebSocket

import Control.Monad

main :: IO ()
main = do
  c <- getConfig
  inject body (run (Server c))
  cache @Post
  cache @Page
  statics c
  forever do
    delay Minute

statics :: Config -> IO ()
statics Config {..} = do
  ws <- clientWS host port
  generateStatic @Page ws
  generateStatic @Post ws

instance Component (Product Post) where
  view Post {..} _ =
    Article <||>
      [ Header  <||> title
      , Section <||> content
      ]

instance Component (Product Page) where
  view Page {..} _ =
    Article <||> content