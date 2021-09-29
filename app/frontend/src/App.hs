{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-orphans #-}
module App (App(..)) where

import Pure.Admin
import Pure.Conjurer
import Pure.Elm.Application hiding (run)
import Pure.Elm.Component hiding (App)
import Pure.Maybe
import Pure.Sync
import Pure.WebSocket

import Shared

data App = App 
  { socket :: WebSocket }

instance Application App where
  data Route App 
    = HomeR 
    | AdminBlogR (ResourceRoute RawPost)
    | AdminPageR (ResourceRoute RawPage)
    | PageR (Slug RawPage)
    | PostR (Slug RawPost)
  
  home = HomeR

  location = \case
    HomeR -> "/"
    AdminBlogR r -> resourceLocation r
    AdminPageR r -> resourceLocation r
    PageR s -> "/" <> toTxt s
    PostR s -> "/blog/" <> toTxt s

  routes = do
    resourceRoutes AdminBlogR
    resourceRoutes AdminPageR
    path "/blog" do
      path "/:post" do
        p <- "post"
        dispatch (PostR p)
    path "/:page" do
      p <- "page"
      dispatch (PageR p)
    dispatch HomeR

  view route App { socket } _ = 
    Div <||>
      [ case route of
        HomeR -> Null

        AdminBlogR r -> 
          resourcePage @Admin socket r

        AdminPageR r -> 
          resourcePage @Admin socket r

        PageR s -> 
          let producer = sync (request pageAPI socket getPage s)
          in producing producer (consuming (maybe "Not Found" run))

        PostR s -> 
          let producer = sync (request postAPI socket getPost s)
          in producing producer (consuming (maybe "Not Found" run))
      ]

instance Theme RawPage
instance Theme RawPost

instance Component (Resource RawPost) where
  view RawPost {..} _ =
    Article <||>
      [ H1  <||> [ txt title ]
      , Div <||> [ txt synopsis ]
      , Div <||> [ txt content ]
      ]

instance Component (Resource RawPage) where
  view RawPage {..} _ =
    Article <||>
      [ H1  <||> [ txt title ]
      , Div <||> [ txt content ]
      ]

instance Component Page where
  view Page {..} _ = 
    Article <||>
      [ Section <||> title
      , Section <||> content
      ]

instance Component Post where
  view Post {..} _ =
    Article <||>
      [ Section <||> title
      , Section <||> content
      ]