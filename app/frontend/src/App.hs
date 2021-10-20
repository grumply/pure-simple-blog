{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-orphans #-}
module App (App(..)) where

import Pure.Admin
import Pure.Conjurer as C
import Pure.Elm.Application hiding (goto,Form)
import Pure.Elm.Component hiding (App)
import qualified Pure.WebSocket as WS

import Shared

data App = App 
  { socket :: WS.WebSocket }

instance Application App where
  data Route App 
    = HomeR 
    | BlogR (C.Route Post)
    | PageR (C.Route Page)
  
  home = HomeR

  location = \case
    HomeR   -> "/"
    BlogR r -> C.location r
    PageR r -> C.location r

  routes = do
    C.routes BlogR
    C.routes PageR
    dispatch HomeR

  view route App { socket } _ = 
    Div <||>
      [ case route of
        HomeR   -> "home"
        BlogR r -> pages @Admin socket r
        PageR r -> pages @Admin socket r
      ]

instance Theme Post
instance Theme Page

instance Fieldable Markdown where
  field onchange initial = 
    Textarea <| OnInput (withInput onchange) |>
      [ txt initial ]

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
  view Page {..} _ = 
    Article <||> 
      [ Section <||> content
      ]

instance Component (Preview Page) where
  view PagePreview {..} _ =
    Article <||>
      [ Section <||> title 
      ]

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
