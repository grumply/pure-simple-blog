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

data Post
instance IsResource Post where

  data Identifier Post = PostName Txt
    deriving stock Generic
    deriving anyclass (ToJSON,FromJSON,Eq,Hashable)

  data Resource Post = RawPost
    { post     :: Txt
    , title    :: Markdown
    , synopsis :: Markdown
    , content  :: Markdown
    } deriving stock Generic
      deriving anyclass (ToJSON,FromJSON,Form)
      
  identifyResource RawPost {..} = PostName post

  data Product Post = Post
    { post     :: Txt
    , title    :: [View]
    , synopsis :: [View]
    , content  :: [View]
    } deriving stock Generic
      deriving anyclass (ToJSON,FromJSON)
      
  identifyProduct Post {..} = PostName post

  data Preview Post = PostPreview
    { post     :: Txt
    , title    :: [View]
    , synopsis :: [View]
    } deriving stock Generic
      deriving anyclass (ToJSON,FromJSON)
      
  identifyPreview PostPreview {..} = PostName post

  route lift = do
    path "/:post" do
      post <- "post"
      dispatch (lift (PostName post))
    continue

  locate (PostName p) = p

data Page
instance IsResource Page where

  data Identifier Page = PageName Txt
    deriving stock Generic
    deriving anyclass (ToJSON,FromJSON,Eq,Hashable)

  data Resource Page = RawPage
    { page    :: Txt
    , title   :: Markdown
    , content :: Markdown
    } deriving stock Generic
      deriving anyclass (ToJSON,FromJSON,Form)
  
  identifyResource RawPage {..} = PageName page

  data Product Page = Page
    { page    :: Txt
    , title   :: [View]
    , content :: [View]
    } deriving stock Generic
      deriving anyclass (ToJSON,FromJSON)
  
  identifyProduct Page {..} = PageName page
      
  data Preview Page = PagePreview
    { page  :: Txt
    , title :: [View]
    } deriving stock Generic
      deriving anyclass (ToJSON,FromJSON)
      
  identifyPreview PagePreview {..} = PageName page

  route lift = do
    path "/:page" do
      page <- "page"
      dispatch (lift (PageName page))
    continue

  locate (PageName p) = p