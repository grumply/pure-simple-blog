{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-orphans #-}
module App (App(..)) where

import Pure.Admin
import Pure.Conjurer
import Pure.Elm.Application hiding (run)
import Pure.Elm.Component hiding (App)
import Pure.WebSocket

import Shared

data App = App 
  { socket :: WebSocket }

instance Application App where
  data Route App 
    = HomeR 
    | BlogR (ResourceRoute Post)
    | PageR (ResourceRoute Page)
  
  home = HomeR

  location = \case
    HomeR   -> "/"
    BlogR r -> resourceLocation r
    PageR r -> resourceLocation r

  routes = do
    resourceRoutes BlogR
    resourceRoutes PageR
    dispatch HomeR

  view route App { socket } _ = 
    Div <||>
      [ case route of
        HomeR   -> Null
        BlogR r -> resourcePage @Admin socket r
        PageR r -> resourcePage @Admin socket r
      ]

instance Theme Page
instance Theme Post

instance Component (Product Page) where
  view Page {..} _ = 
    Article <||>
      [ Section <||> title
      , Section <||> content
      ]

instance Component (Preview Page) where
  view PagePreview {..} _ =
    Article <||>
      [ Section <||> title ]

instance Component (Product Post) where
  view Post {..} _ =
    Article <||>
      [ Section <||> title
      , Section <||> content
      ]

instance Component (Preview Post) where
  view PostPreview {..} _ =
    Article <||>
      [ Section <||> title
      ]