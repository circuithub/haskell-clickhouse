module Database.ClickHouse.Params
  ( Param,

    -- * Parameter types corresponding to Clickhouse's data types
    uint8,
    uint16,
    uint32,
    uint64,
    int8,
    int16,
    int32,
    int64,
    float32,
    double64,
    string,
    Database.ClickHouse.Params.bool,
    uuid,
    day,
    utcTime,

    -- * Running 'Param'
    runParam,
  )
where

import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day, UTCTime, defaultTimeLocale, formatTime)
import Data.UUID (UUID)
import Data.Word (Word16, Word32, Word64, Word8)
import Network.HTTP.Types qualified
import Web.HttpApiData (ToHttpApiData (..))

-- | A serializer for query parameters. @'Param' a@ describes how to turn a
-- value of type @a@ into named query parameters for a ClickHouse query.
--
-- 'Param' is 'Contravariant', 'Semigroup', and 'Monoid' — combine multiple
-- params with '<>' and adapt them to different input types with 'contramap'.
--
-- === Single parameter
--
-- @
-- 'uint64' \"userId\" :: 'Param' 'Data.Word.Word64'
-- @
--
-- === No parameters
--
-- @
-- 'mempty' :: 'Param' ()
-- @
--
-- === Multiple parameters
--
-- @
-- 'Data.Functor.Contravariant.contramap' fst ('uint64' \"id\")
--   '<>' 'Data.Functor.Contravariant.contramap' snd ('string' \"name\")
--   :: 'Param' ('Data.Word.Word64', 'Data.Text.Text')
-- @
newtype Param a = Param
  { runParam :: a -> [Network.HTTP.Types.QueryItem]
  }

instance Semigroup (Param a) where
  Param f <> Param g = Param $ \a ->
    f a <> g a

instance Monoid (Param a) where
  mempty = Param (\_ -> [])

instance Contravariant Param where
  contramap f (Param g) =
    Param (g . f)

-- | Encode an 'Int8' parameter.
--
-- @
-- 'int8' \"x\" :: 'Param' 'Int8'
-- @
int8 :: Text -> Param Int8
int8 = param

-- | Encode an 'Int16' parameter.
--
-- @
-- 'int16' \"x\" :: 'Param' 'Int16'
-- @
int16 :: Text -> Param Int16
int16 = param

-- | Encode an 'Int32' parameter.
--
-- @
-- 'int32' \"x\" :: 'Param' 'Int32'
-- @
int32 :: Text -> Param Int32
int32 = param

-- | Encode an 'Int64' parameter.
--
-- @
-- 'int64' \"x\" :: 'Param' 'Int64'
-- @
int64 :: Text -> Param Int64
int64 = param

-- | Encode a 'Word8' parameter.
--
-- @
-- 'uint8' \"x\" :: 'Param' 'Word8'
-- @
uint8 :: Text -> Param Word8
uint8 = param

-- | Encode a 'Word16' parameter.
--
-- @
-- 'uint16' \"x\" :: 'Param' 'Word16'
-- @
uint16 :: Text -> Param Word16
uint16 = param

-- | Encode a 'Word32' parameter.
--
-- @
-- 'uint32' \"x\" :: 'Param' 'Word32'
-- @
uint32 :: Text -> Param Word32
uint32 = param

-- | Encode a 'Word64' parameter.
--
-- @
-- 'uint64' \"userId\" :: 'Param' 'Word64'
-- @
uint64 :: Text -> Param Word64
uint64 = param

-- | Encode a 'Float' parameter.
--
-- @
-- 'float32' \"score\" :: 'Param' 'Float'
-- @
float32 :: Text -> Param Float
float32 = param

-- | Encode a 'Double' parameter.
--
-- @
-- 'double64' \"price\" :: 'Param' 'Double'
-- @
double64 :: Text -> Param Double
double64 = param

-- | Encode a 'Text' parameter.
--
-- @
-- 'string' \"name\" :: 'Param' 'Text'
-- @
string :: Text -> Param Text
string = param

-- | Encode a 'Bool' parameter.
--
-- @
-- 'bool' \"active\" :: 'Param' 'Bool'
-- @
bool :: Text -> Param Bool
bool = param

-- | Encode a 'UUID' parameter.
--
-- @
-- 'uuid' \"requestId\" :: 'Param' 'UUID'
-- @
uuid :: Text -> Param UUID
uuid = param

-- | Encode a 'Day' parameter.
--
-- @
-- 'day' \"birthDate\" :: 'Param' 'Day'
-- @
day :: Text -> Param Day
day = param

-- | Encode a 'UTCTime' parameter. Formatted as @%Y-%m-%dT%H:%M:%S@.
--
-- @
-- 'utcTime' \"createdAt\" :: 'Param' 'UTCTime'
-- @
utcTime :: Text -> Param UTCTime
utcTime name = Param $ \value ->
  let formatted = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" value
   in [("param_" <> encodeUtf8 name, Just (encodeUtf8 (pack formatted)))]

param :: (ToHttpApiData a) => Text -> Param a
param name = Param $ \value ->
  [("param_" <> encodeUtf8 name, Just (encodeUtf8 (toQueryParam value)))]
