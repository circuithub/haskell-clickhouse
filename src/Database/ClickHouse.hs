module Database.ClickHouse
  ( -- * Connection
    Connection,
    ConnectionOptions (..),
    newConnection,

    -- * Params
    Database.ClickHouse.Params.Param,

    -- * Value
    Database.ClickHouse.Value.Value,

    -- * Result
    Database.ClickHouse.Result.Result,
    Database.ClickHouse.Result.noResult,
    Database.ClickHouse.Result.singleRow,
    Database.ClickHouse.Result.singleRowMaybe,
    Database.ClickHouse.Result.manyRows,

    -- * Insert
    Database.ClickHouse.Insert.Insert,
    Database.ClickHouse.Insert.insert,
    Database.ClickHouse.Insert.modifySettings,

    -- * Running queries and insert
    runInsert,
    runQuery,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource qualified
import Data.ByteString.Builder.Extra qualified
import Data.ByteString.Lazy qualified
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified
import Data.Text.Lazy.Builder qualified
import Database.ClickHouse.Connection
  ( Connection (..),
    ConnectionOptions (..),
    newConnection,
  )
import Database.ClickHouse.Insert qualified
import Database.ClickHouse.Params qualified
import Database.ClickHouse.Result qualified
import Database.ClickHouse.Value qualified
import Network.HTTP.Client qualified
import Network.HTTP.Types qualified

runInsert ::
  (Foldable f) =>
  Connection ->
  Database.ClickHouse.Insert.Insert input value ->
  input ->
  f value ->
  Control.Monad.Trans.Resource.ResourceT IO ()
runInsert connection insert paramsInput inputs = do
  let query =
        Data.Text.Lazy.toStrict
          ( Data.Text.Lazy.Builder.toLazyText
              (Database.ClickHouse.Insert.renderInsert insert)
          )

      request :: Network.HTTP.Client.Request
      request =
        connection.baseRequest
          { Network.HTTP.Client.requestBody =
              Network.HTTP.Client.RequestBodyLBS
                ( Data.ByteString.Builder.Extra.toLazyByteStringWith
                    ( Data.ByteString.Builder.Extra.untrimmedStrategy
                        Data.ByteString.Builder.Extra.defaultChunkSize
                        Data.ByteString.Builder.Extra.defaultChunkSize
                    )
                    mempty
                    (foldMap (Database.ClickHouse.Value.runValue insert.encoder) inputs)
                ),
            Network.HTTP.Client.queryString =
              Network.HTTP.Types.renderQuery
                True
                ( [ ("default_format", Just "RowBinary"),
                    ("query", Just (encodeUtf8 query))
                  ]
                    <> Database.ClickHouse.Params.runParam insert.params paramsInput
                )
          }

  liftIO $
    Network.HTTP.Client.withResponse request connection.manager $ \_response ->
      -- TODO check for any errors
      pure ()

  pure ()

runQuery ::
  Connection ->
  Text ->
  Database.ClickHouse.Params.Param input ->
  Database.ClickHouse.Result.Result output ->
  input ->
  IO output
runQuery Connection {..} query params result input = do
  let request :: Network.HTTP.Client.Request
      request =
        baseRequest
          { Network.HTTP.Client.requestBody =
              Network.HTTP.Client.RequestBodyLBS $
                Data.ByteString.Lazy.fromStrict (encodeUtf8 query),
            Network.HTTP.Client.queryString =
              Network.HTTP.Types.renderQuery
                True
                ( ("default_format", Just "RowBinary")
                    : (Database.ClickHouse.Params.runParam params input)
                )
          }

  Database.ClickHouse.Result.runResult
    result
    (Network.HTTP.Client.responseOpen request manager)
