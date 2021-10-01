{-# OPTIONS_GHC -fno-warn-orphans #-}
module Connection (Connection(..)) where

import Shared
import Markdown

import Pure.Admin as Admin
import Pure.Conjurer
import Pure.Elm.Component as Pure
import Pure.WebSocket as WS

data Connection = Connection 
  { admin  :: Username
  , socket :: WebSocket 
  }

instance Component Connection where
  data Model Connection = Model 
    { token :: Maybe (Token Admin) }

  model = Model Nothing

  data Msg Connection = Startup | AdminTokenMsg AdminTokenMsg

  startup = [Startup]

  upon Startup Connection { admin = a, socket } mdl = do
    enact socket (Admin.admin AdminTokenMsg a)
    enact socket (resourceReadingBackend @Post def def)
    enact socket (resourceReadingBackend @Page def def)
    activate socket
    pure mdl

  upon (AdminTokenMsg tm) Connection { socket } mdl@Model { token } = case tm of
    GetToken withToken -> do
      withToken token 
      pure mdl
    ClearToken -> do
      WS.remove socket (resourcePublishingAPI @Post)
      WS.remove socket (resourcePublishingAPI @Page)
      pure mdl { token = Nothing }
    SetToken t@(Token (un,_)) -> do
      enact socket (resourcePublishingBackend @Post un def def) 
      enact socket (resourcePublishingBackend @Page un def def) 
      pure mdl { token = Just t }

instance Producible Post where
  produce RawPost {..} = pure Post
    { post     = post
    , title    = process title
    , synopsis = process synopsis
    , content  = process content
    }
    
instance Previewable Post where
  preview Post {..} = pure PostPreview
    { post     = post
    , title    = title
    , synopsis = synopsis
    }

instance Producible Page where
  produce RawPage {..} = pure Page
    { page    = page
    , title   = process title
    , content = process content
    }

instance Previewable Page where
  preview Page {..} = pure PagePreview
    { page  = page 
    , title = title
    }
