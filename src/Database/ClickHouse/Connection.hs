{-# LANGUAGE RecordWildCards #-}

module Database.ClickHouse.Connection
  ( Connection (..),
    ConnectionOptions (..),
    newConnection,
  )
where

import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified
import Data.Text.Encoding qualified
import Network.HTTP.Client (Request (..))
import Network.HTTP.Client qualified as HTTP

-- | An open connection to a ClickHouse server.
data Connection = Connection
  { baseRequest :: !HTTP.Request,
    manager :: !HTTP.Manager
  }

-- | Options for establishing a 'Connection' to a ClickHouse server.
--
-- All fields except 'url' are optional.
data ConnectionOptions = ConnectionOptions
  { -- | Base URL of the ClickHouse server (e.g. @\"http://localhost:8123\"@).
    url :: !Text,
    -- | Database name to use.
    database :: !(Maybe Text),
    -- | User name for authentication.
    user :: !(Maybe Text),
    -- | Password for authentication.
    password :: !(Maybe Text),
    -- | Optional pre-existing connection manager. When 'Nothing', a new one is created.
    httpManager :: !(Maybe HTTP.Manager),
    -- | HTTP response timeout in seconds. Defaults to @10@ seconds when 'Nothing'.
    responseTimeoutSeconds :: !(Maybe Int)
  }

-- | Create a new 'Connection' from the given 'ConnectionOptions'.
--
-- @
-- connection <-
--   'newConnection'
--     'ConnectionOptions'
--       { url = \"http:\/\/localhost:8123\",
--         database = Nothing,
--         user = Nothing,
--         password = Nothing,
--         httpManager = Nothing,
--         responseTimeoutSeconds = Nothing
--       }
-- @
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
                let seconds = maybe 10 id responseTimeoutSeconds
                 in HTTP.responseTimeoutMicro (seconds * 1_000_000)
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
