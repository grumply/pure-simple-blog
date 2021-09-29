module Main where

import Pure.Auth
import Pure.Admin
import Pure.Elm.Component as Component
import Pure.Elm.Application as Application
import Pure.WebSocket

import App

main :: IO ()
main = do
  ws <- clientWS "127.0.0.1" 8081
  inject body (Component.run (Access ws def def :: Access Admin))
  inject body (Application.run (App.App ws))
