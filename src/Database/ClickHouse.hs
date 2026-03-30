-- |
-- = Connecting
--
-- @
-- connection <-
--   'newConnection'
--     'ConnectionOptions'
--       { url = \"http:\/\/localhost:8123\",
--         database = Nothing,
--         user = Nothing,
--         password = Nothing,
--         httpManager = Nothing
--       }
-- @
--
-- = Running queries
--
-- Use 'runQuery' with a SQL query, 'Database.ClickHouse.Params.Param' for
-- parameters, and a 'Database.ClickHouse.Result.Result' to describe the
-- expected output.
--
-- == Querying a single row
--
-- Use @{name:Type}@ placeholders in SQL and match them to parameters:
--
-- @
-- import "Database.ClickHouse.Params" qualified as Params
-- import "Database.ClickHouse.Result" qualified as Result
--
-- result <-
--   'runQuery'
--     connection
--     \"SELECT { x : UInt64 }\"
--     (Params.uint64 \"x\")
--     ('Database.ClickHouse.Result.singleRow' (Result.column Result.uint64))
--     42
-- @
--
-- == Querying multiple columns
--
-- 'Database.ClickHouse.Result.Row' is 'Applicative', so combine columns with
-- @\<$\>@ and @\<*\>@:
--
-- @
-- result <-
--   'runQuery'
--     connection
--     \"SELECT name, age FROM users WHERE id = { userId : UInt64 }\"
--     (Params.uint64 \"userId\")
--     ('Database.ClickHouse.Result.singleRow'
--       ((,)
--         \<$\> Result.column Result.string
--         \<*\> Result.column Result.uint32))
--     1
-- @
--
-- == Querying many rows
--
-- @
-- rows <-
--   'runQuery'
--     connection
--     \"SELECT name FROM users\"
--     'mempty'
--     ('Database.ClickHouse.Result.manyRows' (Result.column Result.string))
--     ()
-- @
--
-- == Queries without parameters
--
-- Use 'mempty' for the parameters and @()@ for the input:
--
-- @
-- 'runQuery' connection \"SELECT 1\" 'mempty' ('Database.ClickHouse.Result.noResult') ()
-- @
--
-- = Inserting data
--
-- Build an 'Database.ClickHouse.Internal.Insert.Insert' describing the target table,
-- columns, and row encoder, then run it with 'runInsert'.
--
-- == Inserting a single-column row
--
-- @
-- import "Database.ClickHouse.Value" qualified as Value
--
-- let ins = 'insert' \"my_table\" [\"val\"] Value.uint32 'mempty'
-- 'runInsert' connection ins () [1, 2, 3 :: Word32]
-- @
--
-- == Inserting multi-column rows
--
-- Combine 'Database.ClickHouse.Value.Value' encoders with 'Data.Functor.Contravariant.contramap'
-- and '<>':
--
-- @
-- import Data.Functor.Contravariant ('Data.Functor.Contravariant.contramap')
--
-- let encoder =
--       'Data.Functor.Contravariant.contramap' fst Value.int64
--         \<\> 'Data.Functor.Contravariant.contramap' snd Value.string
--
-- let ins = 'insert' \"events\" [\"id\", \"name\"] encoder 'mempty'
-- 'runInsert' connection ins () [(1, \"click\"), (2, \"view\")]
-- @
--
-- == Inserting nullable and nested types
--
-- @
-- let encoder =
--       'Data.Functor.Contravariant.contramap' fst Value.string
--         \<\> 'Data.Functor.Contravariant.contramap' snd (Value.nullable Value.uint32)
--
-- let ins = 'insert' \"users\" [\"name\", \"age\"] encoder 'mempty'
-- 'runInsert' connection ins () [(\"alice\", Just 30), (\"bob\", Nothing)]
-- @
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
    Database.ClickHouse.Internal.Insert.Insert,
    Database.ClickHouse.Internal.Insert.insert,
    Database.ClickHouse.Internal.Insert.modifySettings,

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
import Database.ClickHouse.Internal.Insert qualified
import Database.ClickHouse.Params qualified
import Database.ClickHouse.Result qualified
import Database.ClickHouse.Stream (Stream (..), ToStreamIO (..))
import Database.ClickHouse.Value qualified
import Network.HTTP.Client qualified
import Network.HTTP.Types qualified

-- | Execute an 'Database.ClickHouse.Internal.Insert.Insert' statement, streaming rows
-- into ClickHouse.
--
-- The @values@ argument can be any type with a 'ToStreamIO' instance (e.g. a
-- list or a 'Database.ClickHouse.Stream.Stream').
runInsert ::
  (ToStreamIO value values) =>
  Connection ->
  Database.ClickHouse.Internal.Insert.Insert input value ->
  input ->
  values ->
  IO ()
runInsert connection insert paramsInput inputs = do
  let query =
        Data.Text.Lazy.toStrict
          ( Data.Text.Lazy.Builder.toLazyText
              (Database.ClickHouse.Internal.Insert.renderInsert insert)
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

-- | Execute a query against ClickHouse and deserialize the response.
--
-- The query is a plain SQL 'Text'. Use @{name:Type}@ placeholders together
-- with a 'Database.ClickHouse.Params.Param' to safely pass parameters.
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
