{-# language DerivingStrategies, RecordWildCards, TypeFamilies, DeriveGeneric, DeriveAnyClass, CPP, DuplicateRecordFields #-}
module Shared where

import Pure.Data.Txt
import Pure.Data.JSON
import Pure.Conjurer
import Pure.Conjurer.Form
import Pure.Elm.Component hiding (pattern Delete)
import Pure.Data.Render ()
import Pure.Sorcerer
import Pure.WebSocket as WS

import Data.Hashable
import Data.Typeable
import GHC.Generics

newtype Markdown = Markdown Txt
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON,Eq)
  deriving (ToTxt,FromTxt) via Txt

instance Field Markdown where
  field onchange initial = 
    Textarea <| OnInput (withInput onchange) |>
      [ txt initial ]

data RawPost deriving Typeable
instance IsResource RawPost where
  data Resource RawPost = RawPost
    { url      :: Txt
    , title    :: Markdown
    , synopsis :: Markdown
    , content  :: Markdown
    } deriving stock Generic
      deriving anyclass (ToJSON,FromJSON,Form)

  root = "/admin_blog"

  slug RawPost {..} = toSlug url

data Post = Post
  { title    :: [View]
  , synopsis :: [View]
  , content  :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data PostMsg
  = PostCreated Post
  | PostUpdated Post
  | PostDeleted
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Source PostMsg where
  data Stream PostMsg = PostStream (Slug RawPost)
    deriving stock Generic
    deriving anyclass Hashable

instance Aggregable PostMsg Post where
  update (PostCreated p) Nothing = Update p
  update (PostUpdated p) (Just _) = Update p
  update PostDeleted (Just _) = Delete
  update _ _ = Ignore

mkRequest "GetPost"
  [t|Slug RawPost -> Maybe Post|]

postAPI = api msgs reqs
  where
    msgs = WS.none
    reqs = getPost <:> WS.none

data RawPage deriving Typeable
instance IsResource RawPage where
  data Resource RawPage = RawPage
    { url     :: Txt
    , title   :: Markdown
    , content :: Markdown
    } deriving stock Generic
      deriving anyclass (ToJSON,FromJSON,Form)

  root = "/admin_pages"

  slug RawPage {..} = toSlug url

data Page = Page
  { title   :: [View]
  , content :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data PageMsg
  = PageCreated Page
  | PageUpdated Page
  | PageDeleted
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Source PageMsg where
  data Stream PageMsg = PageStream (Slug RawPage)
    deriving stock Generic
    deriving anyclass Hashable

instance Aggregable PageMsg Page where
  update (PageCreated p) Nothing = Update p
  update (PageUpdated p) (Just _) = Update p
  update PageDeleted (Just _) = Delete
  update _ _ = Ignore

mkRequest "GetPage"
  [t|Slug RawPage -> Maybe Page|]

pageAPI = api msgs reqs
  where
    msgs = WS.none
    reqs = getPage <:> WS.none