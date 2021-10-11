{-# OPTIONS_GHC -fno-warn-orphans #-}
module Connection (Connection(..)) where

import Shared
import Markdown

import Pure.Admin as Admin
import Pure.Conjurer
import Pure.Elm.Component as Pure hiding (render)
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
    enact socket (cachingReading @Post readPermissions def)
    enact socket (cachingReading @Page readPermissions def)
    activate socket
    pure mdl

  upon (AdminTokenMsg tm) Connection { socket } mdl@Model { token } = case tm of
    GetToken withToken -> do
      withToken token 
      pure mdl
    ClearToken -> do
      WS.remove socket (publishingAPI @Post)
      WS.remove socket (publishingAPI @Page)
      pure mdl { token = Nothing }
    SetToken t@(Token (un,_)) -> do
      WS.remove socket (publishingAPI @Post)
      WS.remove socket (publishingAPI @Page)
      enact socket (cachingPublishing @Post fullPermissions def) 
      enact socket (cachingPublishing @Page fullPermissions def) 
      pure mdl { token = Just t }

instance Processable Post

instance Producible Post where
  produce RawPost {..} = pure Post
    { title    = render title
    , content  = render content
    }
    
instance Previewable Post where
  preview RawPost { synopsis } Post { title } = pure PostPreview
    { title    = title
    , synopsis = render synopsis
    }

instance Processable Page

instance Producible Page where
  produce RawPage {..} = pure Page
    { content = render content
    }

instance Previewable Page where
  preview RawPage { title } _ = pure PagePreview
    { title = render title
    }
