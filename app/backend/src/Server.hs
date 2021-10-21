{-# language DuplicateRecordFields #-}
module Server (Server(..)) where

import Connection 
import Server.Config
import Shared

import Pure.Admin as Admin
import Pure.Conjurer
import Pure.Elm.Component
import qualified Pure.Server as Pure
import Pure.Sorcerer

data Server = Server Config

instance Component Server where
  data Model Server = Model
    
  initialize (Server Config {..}) = do
    Admin.initialize admin password
    pure Model

  view (Server Config {..}) Model = 
    Div <||>
      [ sorcerer (adminDB ++ db @Post ++ db @Page)
      , Pure.Server host port (run . Connection admin)
      ]