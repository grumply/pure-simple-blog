{-# language DuplicateRecordFields #-}
module Server 
  ( Server(..)
  ) where

import Connection 
import Server.Config
import Shared

import Pure.Admin as Admin
import Pure.Conjurer
import Pure.Elm.Component
import qualified Pure.Server as Pure
import Pure.Sorcerer

data Server = Server

instance Component Server where
  data Model Server = Model
    { admin :: Username
    , host  :: String
    , port  :: Int
    } 
    
  initialize _ = do
    Config {..} <- getConfig
    Admin.initialize username password
    pure Model {..}

  view _ Model {..} = 
    Div <||>
      [ sorcerer db
      , Pure.Server host port (run . Connection admin)
      ]

db = concat 
  [adminDB
  ,[listener @PostMsg @Post]
  ,[listener @PageMsg @Page]
  ,resourceDB @RawPost
  ,resourceDB @RawPage
  ]