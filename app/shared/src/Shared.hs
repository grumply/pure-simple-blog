{-# language DerivingStrategies, RecordWildCards, TypeFamilies, DeriveGeneric, DeriveAnyClass, CPP, DuplicateRecordFields #-}
module Shared where

import Pure.Conjurer
import Pure.Conjurer.Form
import Pure.Data.Txt
import Pure.Data.JSON
import Pure.Data.Render ()
import Pure.Elm.Component hiding (pattern Delete)
import Pure.Router

import Data.Hashable
import GHC.Generics

newtype Markdown = Markdown Txt
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON,Eq)
  deriving (ToTxt,FromTxt) via Txt

instance Field Markdown where
  field onchange initial = 
    Textarea <| OnInput (withInput onchange) |>
      [ txt initial ]

--------------------------------------------------------------------------------
-- Post

data Post
data instance Identifier Post = PostName Txt
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON,Eq,Hashable)

data instance Resource Post = RawPost
  { post     :: Txt
  , title    :: Markdown
  , synopsis :: Markdown
  , content  :: Markdown
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON,Form)

instance Identifiable Resource Post where 
  identify RawPost {..} = PostName post

data instance Product Post = Post
  { title    :: [View]
  , content  :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Preview Post = PostPreview
  { post     :: Txt
  , title    :: [View]
  , synopsis :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

instance Identifiable Preview Post where
  identify PostPreview {..} = PostName post

instance Routable Post where
  route lift = do
    path "/:post" do
      post <- "post"
      dispatch (lift (PostName post))
    continue

  locate (PostName p) = p

--------------------------------------------------------------------------------
-- Page

data Page
data instance Identifier Page = PageName Txt
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON,Eq,Hashable)

data instance Resource Page = RawPage
  { page    :: Txt
  , title   :: Markdown
  , content :: Markdown
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON,Form)

instance Identifiable Resource Page where
  identify RawPage {..} = PageName page

data instance Product Page = Page
  { content :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)
   
data instance Preview Page = PagePreview
  { page  :: Txt
  , title :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

instance Identifiable Preview Page where 
  identify PagePreview {..} = PageName page

instance Routable Page where
  route lift = do
    path "/:page" do
      page <- "page"
      dispatch (lift (PageName page))
    continue

  locate (PageName p) = p