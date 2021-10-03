{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-orphans #-}
module App (App(..)) where

import Pure.Admin
import Pure.Conjurer
import Pure.Elm.Application hiding (goto,run)
import Pure.Elm.Component hiding (App)
import qualified Pure.WebSocket as WS

import Shared
import GHC.Generics

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
        HomeR   -> Null
        BlogR r -> resourcePage @Admin socket r
        PageR r -> resourcePage @Admin socket r
      ]

instance Pathable (Identifier Page)
instance Readable Page
instance Creatable Admin Page where
  data CreateContext Admin Page = CreatePageContext
    deriving stock Generic
    deriving anyclass Pathable
instance Listable Page

instance Pathable (Identifier Post)
instance Readable Post
instance Creatable Admin Post where
  data CreateContext Admin Post = CreatePostContext
    deriving stock Generic
    deriving anyclass Pathable
instance Listable Post

instance Component (Product Page) where
  view Page {..} _ = Article <||> content

instance Component (Preview Page) where
  view PagePreview {..} _ =
    Article <| OnClick (\_ -> goto (ReadR @Admin (Read "admin" (PageName page)))) |>
      [ Section <||> title ]

instance Component (Product Post) where
  view Post {..} _ =
    Article <||>
      [ Section <||> title
      , Section <||> content
      ]

instance Component (Preview Post) where
  view PostPreview {..} _ =
    Article <| OnClick (\_ -> goto (ReadR @Admin (Read "admin" (PostName post)))) |>
      [ Section <||> title
      ]