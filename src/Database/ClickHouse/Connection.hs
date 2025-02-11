{-# LANGUAGE RecordWildCards #-}

module Database.ClickHouse.Connection
  ( Connection (..),
    ConnectionOptions (..),
    newConnection,
  )
where

import Control.Exception.Safe (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified
import Data.Text.Encoding qualified
import Network.HTTP.Client (Request (..))
import Network.HTTP.Client qualified as HTTP

data Connection = Connection
  { baseRequest :: !HTTP.Request,
    manager :: !HTTP.Manager
  }

data ConnectionOptions = ConnectionOptions
  { url :: !Text,
    database :: !(Maybe Text),
    user :: !(Maybe Text),
    password :: !(Maybe Text),
    httpManager :: !(Maybe HTTP.Manager)
  }

newConnection :: (MonadThrow m, MonadIO m) => ConnectionOptions -> m Connection
newConnection ConnectionOptions {..} = do
  request <- HTTP.parseRequest (Data.Text.unpack url)

  let headers =
        catMaybes
          [ database <&> \database -> ("X-ClickHouse-Database", Data.Text.Encoding.encodeUtf8 database),
            user <&> \user -> ("X-ClickHouse-User", Data.Text.Encoding.encodeUtf8 user),
            password <&> \password -> ("X-ClickHouse-Key", Data.Text.Encoding.encodeUtf8 password)
          ]
          <> [("X-ClickHouse-Format", "RowBinary")]

      baseRequest =
        HTTP.setRequestCheckStatus $
          request
            { method = "POST",
              requestHeaders = headers,
              responseTimeout =
                -- 15 minute response timeout
                HTTP.responseTimeoutMicro (15 * 60 * 1_000_000)
            }

  manager <-
    case httpManager of
      Just httpManager ->
        pure httpManager
      Nothing ->
        liftIO $
          HTTP.newManager HTTP.defaultManagerSettings

  pure $
    Connection
      { baseRequest,
        manager
      }
