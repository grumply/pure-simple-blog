module Connection 
  ( Connection(..)
  ) where

import Shared
import Markdown

import Pure.Admin as Admin
import Pure.Conjurer
import Pure.Elm.Component as Pure
import Pure.WebSocket as WS
import Pure.Sorcerer as Sorcerer

import Control.Monad

data Connection = Connection { admin :: Username, socket :: WebSocket }

instance Component Connection where
  data Model Connection = Model { adminToken :: Maybe (Token Admin) }

  model = Model Nothing

  data Msg Connection = Startup | AdminTokenMsg AdminTokenMsg

  startup = [Startup]

  upon Startup Connection { admin = a, socket } mdl = do
    enact socket (Admin.admin AdminTokenMsg a)
    enact socket (resourceBackend @RawPost postPermissions postCallbacks) 
    enact socket (resourceBackend @RawPage pagePermissions pageCallbacks) 
    enact socket post
    enact socket page
    activate socket
    pure mdl

  upon (AdminTokenMsg tm) _ mdl@Model { adminToken } = case tm of
    GetToken withToken -> withToken adminToken >> pure mdl
    ClearToken -> pure mdl { adminToken = Nothing }
    SetToken t -> pure mdl { adminToken = Just t }

postPermissions :: Elm (Msg Connection) => Permissions RawPost
postPermissions = Permissions {..}
  where
    canCreate _ = isAdmin AdminTokenMsg
    canRead   _ = isAdmin AdminTokenMsg
    canUpdate _ = isAdmin AdminTokenMsg
    canDelete _ = isAdmin AdminTokenMsg
    canList     = isAdmin AdminTokenMsg

postCallbacks :: Elm (Msg Connection) => Callbacks RawPost
postCallbacks = Callbacks {..}
  where
    -- TODO: log failure (case where isNothing)
    -- TODO: add tagging and tag updates here
    onCreate ttl = traverse_ $ \RawPost {..} ->
      Sorcerer.write (PostStream ttl) $ PostCreated Post
        { title    = process title
        , synopsis = process synopsis
        , content  = process content
        }

    onRead   _ _ = def

    -- TODO: add tagging and tag updates here
    onUpdate ttl = traverse_ $ \RawPost {..} ->
      Sorcerer.write (PostStream ttl) $ PostUpdated Post
        { title    = process title
        , synopsis = process synopsis
        , content  = process content
        }

    onDelete ttl deleted = 
      when deleted do
        Sorcerer.write (PostStream ttl) PostDeleted

    onList       = def

pagePermissions :: Elm (Msg Connection) => Permissions RawPage
pagePermissions = Permissions {..}
  where
    canCreate _ = isAdmin AdminTokenMsg
    canRead   _ = isAdmin AdminTokenMsg
    canUpdate _ = isAdmin AdminTokenMsg
    canDelete _ = isAdmin AdminTokenMsg
    canList     = isAdmin AdminTokenMsg

pageCallbacks :: Elm (Msg Connection) => Callbacks RawPage
pageCallbacks = Callbacks {..}
  where
    onCreate ttl = traverse_ $ \RawPage {..} ->
      Sorcerer.write (PageStream ttl) $ PageCreated Page
        { title   = process title
        , content = process content
        }

    onRead   _ _ = def

    onUpdate ttl = traverse_ $ \RawPage {..} ->
      Sorcerer.write (PageStream ttl) $ PageUpdated Page
        { title   = process title
        , content = process content
        }
      
    onDelete ttl deleted =
      when deleted do
        Sorcerer.write (PageStream ttl) PageDeleted

    onList       = def 

post = Endpoints postAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleGetPost <:> WS.none

handleGetPost :: RequestHandler GetPost
handleGetPost = responding do
  slug <- acquire
  Sorcerer.read (PostStream slug) >>= reply

page = Endpoints pageAPI msgs reqs
  where
    msgs = WS.none
    reqs = handleGetPage <:> WS.none

handleGetPage :: RequestHandler GetPage
handleGetPage = responding do
  slug <- acquire
  Sorcerer.read (PageStream slug) >>= reply