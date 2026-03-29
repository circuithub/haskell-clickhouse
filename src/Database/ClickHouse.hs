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

    -- * Streaming
    Database.ClickHouse.Stream.ToStreamIO (..),

    -- * Running queries and insert
    runInsert,
    runQuery,
  )
where

import Data.ByteString qualified
import Data.ByteString.Builder.Extra qualified
import Data.ByteString.Lazy qualified
import Data.IORef
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
import Database.ClickHouse.Stream (Stream (..), ToStreamIO (..))
import Database.ClickHouse.Value qualified
import Network.HTTP.Client qualified
import Network.HTTP.Types qualified

runInsert ::
  (ToStreamIO value values) =>
  Connection ->
  Database.ClickHouse.Insert.Insert input value ->
  input ->
  values ->
  IO ()
runInsert connection insert paramsInput inputs = do
  let query =
        Data.Text.Lazy.toStrict
          ( Data.Text.Lazy.Builder.toLazyText
              (Database.ClickHouse.Insert.renderInsert insert)
          )

      stream = toStreamIO inputs

      request :: Network.HTTP.Client.Request
      request =
        connection.baseRequest
          { Network.HTTP.Client.requestBody =
              streamToRequestBody insert.encoder stream,
            Network.HTTP.Client.queryString =
              Network.HTTP.Types.renderQuery
                True
                ( [ ("default_format", Just "RowBinary"),
                    ("query", Just (encodeUtf8 query))
                  ]
                    <> Database.ClickHouse.Params.runParam insert.params paramsInput
                )
          }

  Network.HTTP.Client.withResponse request connection.manager $ \_response ->
    -- TODO check for any errors
    pure ()

streamToRequestBody :: Database.ClickHouse.Value.Value a -> Stream IO a -> Network.HTTP.Client.RequestBody
streamToRequestBody encoder stream =
  Network.HTTP.Client.RequestBodyStreamChunked $ \needsPopper -> do
    ref <- newIORef (Encode stream)
    needsPopper (popper ref)
  where
    popper ref = do
      step <- readIORef ref
      nextStep ref step

    nextStep _ref Done = pure Data.ByteString.empty
    nextStep ref (Encode s) =
      unStream
        s
        ( \value rest ->
            nextStep ref $
              Yield
                ( Data.ByteString.Lazy.toChunks
                    ( Data.ByteString.Builder.Extra.toLazyByteStringWith
                        ( Data.ByteString.Builder.Extra.untrimmedStrategy
                            Data.ByteString.Builder.Extra.defaultChunkSize
                            Data.ByteString.Builder.Extra.defaultChunkSize
                        )
                        mempty
                        (Database.ClickHouse.Value.runValue encoder value)
                    )
                )
                rest
        )
        (pure Data.ByteString.empty)
    nextStep ref (Yield chunks rest) =
      case chunks of
        [] -> nextStep ref (Encode rest)
        (x : xs)
          | Data.ByteString.null x ->
              nextStep ref (Yield xs rest)
          | otherwise -> do
              writeIORef ref (Yield xs rest)
              pure x

data PopperStep a
  = Encode (Stream IO a)
  | Yield [Data.ByteString.ByteString] (Stream IO a)
  | Done

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
