{-# language DerivingStrategies, RecordWildCards, TypeFamilies, DeriveGeneric, DeriveAnyClass, CPP, DuplicateRecordFields #-}
module Shared where

import Pure.Conjurer
import Pure.Data.Txt
import Pure.Data.JSON
import Pure.Data.Render ()
import Pure.Elm.Component hiding (pattern Delete)

import Data.Hashable

import GHC.Generics

newtype Markdown = Markdown Txt
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON,Eq)
  deriving (ToTxt,FromTxt) via Txt

data Post
data instance Resource Post = RawPost
  { title    :: Markdown
  , synopsis :: Markdown
  , content  :: Markdown
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON,Default)

data instance Context Post = PostContext
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON,Pathable,Hashable)

data instance Name Post = PostName (Slug Post)
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON,Pathable,Hashable)

instance Routable Post

instance Nameable Post where
  toName RawPost {..} = PostName (fromTxt (toTxt title))
  
data instance Product Post = Post
  { title    :: [View]
  , content  :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Preview Post = PostPreview
  { title    :: [View]
  , synopsis :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data Page
data instance Resource Page = RawPage
  { title   :: Markdown
  , content :: Markdown
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON,Default)

data instance Context Page = PageContext
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON,Pathable,Hashable)

data instance Name Page = PageName (Slug Page)
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON,Pathable,Hashable)

instance Routable Page

instance Nameable Page where
  toName RawPage {..} = PageName (fromTxt (toTxt title))

data instance Product Page = Page
  { content :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)
   
data instance Preview Page = PagePreview
  { title :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)
