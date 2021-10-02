{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-orphans #-}
module App (App(..)) where

import Pure.Admin
import Pure.Conjurer
import Pure.Elm.Application hiding (goto,run)
import Pure.Elm.Component hiding (App)
import qualified Pure.WebSocket as WS

import Shared

data App = App 
  { socket :: WS.WebSocket }

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
    path "/blog" do
      path "/:post" do
        post <- "post"
        dispatch (BlogR (ReadProduct "admin" (PostName post)))
      dispatch (BlogR (ListPreviews (Just "admin")))
    path "/:page" do
      page <- "page"
      dispatch (PageR (ReadProduct "admin" (PageName page)))
    dispatch HomeR

  view route App { socket } _ = 
    Div <||>
      [ case route of
        HomeR   -> Null
        BlogR r -> resourcePage @Admin socket r
        PageR r -> resourcePage @Admin socket r
      ]

instance Component (Product Page) where
  view Page {..} _ = Article <||> content

instance Component (Preview Page) where
  view PagePreview {..} _ =
    Article <| OnClick (\_ -> goto (ReadProduct "admin" (PageName page))) |>
      [ Section <||> title ]

instance Component (Product Post) where
  view Post {..} _ =
    Article <||>
      [ Section <||> title
      , Section <||> content
      ]

instance Component (Preview Post) where
  view PostPreview {..} _ =
    Article <| OnClick (\_ -> goto (ReadProduct "admin" (PostName post))) |>
      [ Section <||> title
      ]