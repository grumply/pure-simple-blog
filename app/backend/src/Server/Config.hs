{-# language DeriveAnyClass #-}
module Server.Config 
  ( Config(..)
  , getConfig
  ) where

import Pure.Admin
import Pure.Data.JSON (ToJSON,FromJSON)

import Data.Yaml (decodeFileThrow)

import GHC.Generics

data Config = Config
  { host     :: String
  , port     :: Int
  , username :: Username
  , password :: Password
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

getConfig :: IO Config
getConfig = decodeFileThrow "config.yaml"