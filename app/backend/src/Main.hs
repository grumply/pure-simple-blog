module Main where

import Pure.Elm.Component
import Server

import Control.Monad

main :: IO ()
main = inject body (run Server) >> keepalive
  where keepalive = forever (delay Minute)