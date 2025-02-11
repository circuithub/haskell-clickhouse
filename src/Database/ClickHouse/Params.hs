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

    -- * Running 'Param'
    runParam,
  )
where

import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day)
import Data.UUID (UUID)
import Data.Word (Word16, Word32, Word64, Word8)
import Network.HTTP.Types qualified
import Web.HttpApiData (ToHttpApiData (..))

-- | A combinator type that allows for building flexible serializers for query
-- parameter values.
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

int8 :: Text -> Param Int8
int8 = param

int16 :: Text -> Param Int16
int16 = param

int32 :: Text -> Param Int32
int32 = param

int64 :: Text -> Param Int64
int64 = param

uint8 :: Text -> Param Word8
uint8 = param

uint16 :: Text -> Param Word16
uint16 = param

uint32 :: Text -> Param Word32
uint32 = param

uint64 :: Text -> Param Word64
uint64 = param

float32 :: Text -> Param Float
float32 = param

double64 :: Text -> Param Double
double64 = param

string :: Text -> Param Text
string = param

bool :: Text -> Param Bool
bool = param

uuid :: Text -> Param UUID
uuid = param

day :: Text -> Param Day
day = param

param :: (ToHttpApiData a) => Text -> Param a
param name = Param $ \value ->
  [("param_" <> encodeUtf8 name, Just (encodeUtf8 (toQueryParam value)))]
