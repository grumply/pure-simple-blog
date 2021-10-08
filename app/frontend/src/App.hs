{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-orphans #-}
module App (App(..)) where

import Pure.Admin
import Pure.Conjurer
import Pure.Elm.Application hiding (goto,Form)
import Pure.Elm.Component hiding (App)
import qualified Pure.WebSocket as WS

import Shared

data App = App 
  { socket :: WS.WebSocket }

instance Application App where
  data Route App 
    = HomeR 
    | BlogR (ResourceRoute Admin Post)
    | PageR (ResourceRoute Admin Page)
  
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
        HomeR   -> "home"
        BlogR r -> resourcePages @Admin socket r
        PageR r -> resourcePages @Admin socket r
      ]

instance Readable Page
instance Formable (Resource Page)
instance Creatable Admin Page
instance Updatable Admin Page
instance Listable Page

instance Readable Post
instance Formable (Resource Post)
instance Creatable Admin Post
instance Updatable Admin Post
instance Listable Post

instance Component (Product Page) where
  view Page {..} _ = Article <||> content

instance Component (KeyedPreview Page) where
  view (KeyedPreview ctx name PagePreview {..}) _ =
    Article <| OnClick (\_ -> goto (ReadR ctx name)) |>
      [ Section <||> title 
      ]

instance Component (Product Post) where
  view Post {..} _ =
    Article <||>
      [ Section <||> title
      , Section <||> content
      ]

instance Component (KeyedPreview Post) where
  view (KeyedPreview ctx name PostPreview {..}) _ =
    Article <| OnClick (\_ -> goto (ReadR @Admin ctx name)) |>
      [ Section <||> title
      ]