module Database.ClickHouse.Value
  ( Value,
    runValue,
    int8,
    uint8,
    int16,
    uint16,
    int32,
    uint32,
    int64,
    uint64,
    float32,
    float64,
    string,
    uuid,
    Database.ClickHouse.Value.bool,
    Database.ClickHouse.Value.map,
    array,
    dateTime,
    dateTime32,
    dateTime64,
    date,
    date32,
    nullable,
  )
where

import Data.Bits qualified
import Data.ByteString.Builder qualified
import Data.Functor.Contravariant (Contravariant (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import Data.Text.Encoding qualified
import Data.Text.Foreign qualified
import Data.Time qualified
import Data.Time.Calendar qualified
import Data.Time.Clock.POSIX qualified
import Data.Time.Clock.System qualified
import Data.UUID qualified
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Exts qualified

newtype Value a = Value (a -> Data.ByteString.Builder.Builder)
  deriving newtype (Semigroup)

instance Contravariant Value where
  contramap f (Value g) =
    Value (\x -> g (f x))

runValue :: Value a -> a -> Data.ByteString.Builder.Builder
runValue (Value f) x = f x

int8 :: Value Int8
int8 = Value Data.ByteString.Builder.int8
{-# INLINE int8 #-}

uint8 :: Value Word8
uint8 = Value Data.ByteString.Builder.word8
{-# INLINE uint8 #-}

int16 :: Value Int16
int16 = Value Data.ByteString.Builder.int16LE
{-# INLINE int16 #-}

uint16 :: Value Word16
uint16 = Value Data.ByteString.Builder.word16LE
{-# INLINE uint16 #-}

int32 :: Value Int32
int32 = Value Data.ByteString.Builder.int32LE
{-# INLINE int32 #-}

uint32 :: Value Word32
uint32 = Value Data.ByteString.Builder.word32LE
{-# INLINE uint32 #-}

int64 :: Value Int64
int64 = Value Data.ByteString.Builder.int64LE
{-# INLINE int64 #-}

uint64 :: Value Word64
uint64 = Value Data.ByteString.Builder.word64LE
{-# INLINE uint64 #-}

string :: Value Text
string = Value $ \text ->
  encodeLEB128 (fromIntegral (Data.Text.Foreign.lengthWord8 text))
    <> Data.Text.Encoding.encodeUtf8Builder text
{-# INLINE string #-}

uuid :: Value Data.UUID.UUID
uuid = Value $ \uuid ->
  case Data.UUID.toWords64 uuid of
    (lo, hi) ->
      Data.ByteString.Builder.word64LE lo <> Data.ByteString.Builder.word64LE hi
{-# INLINE uuid #-}

map :: (GHC.Exts.IsList f, GHC.Exts.Item f ~ (a, b)) => Value a -> Value b -> Value f
map (Value f) (Value g) =
  contramap
    GHC.Exts.toList
    (array tuple2)
  where
    tuple2 = Value $ \(a, b) -> f a <> g b
{-# INLINE map #-}

array :: (Foldable f) => Value a -> Value (f a)
array (Value f) = Value $ \xs ->
  encodeLEB128 (fromIntegral (length xs)) <> foldMap f xs
{-# INLINE array #-}

float32 :: Value Float
float32 = Value Data.ByteString.Builder.floatLE
{-# INLINE float32 #-}

float64 :: Value Double
float64 = Value Data.ByteString.Builder.doubleLE
{-# INLINE float64 #-}

bool :: Value Bool
bool = Value $ \x ->
  Data.ByteString.Builder.word8 (if x then 1 else 0)
{-# INLINE bool #-}

nullable :: Value a -> Value (Maybe a)
nullable (Value f) = Value $ \x ->
  case x of
    Nothing -> Data.ByteString.Builder.word8 1
    Just x -> Data.ByteString.Builder.word8 0 <> f x
{-# INLINE nullable #-}

dateTime :: Value Data.Time.UTCTime
dateTime = dateTime32
{-# INLINE dateTime #-}

dateTime32 :: Value Data.Time.UTCTime
dateTime32 = Value $ \time ->
  Data.ByteString.Builder.int32LE $!
    round (Data.Time.Clock.POSIX.utcTimeToPOSIXSeconds time)
{-# INLINE dateTime32 #-}

dateTime64 :: Value Data.Time.UTCTime
dateTime64 = Value $ \time ->
  Data.ByteString.Builder.int64LE $!
    round (Data.Time.Clock.POSIX.utcTimeToPOSIXSeconds time)
{-# INLINE dateTime64 #-}

date :: Value Data.Time.Day
date = Value $ \date ->
  Data.ByteString.Builder.int16LE $!
    fromIntegral $
      date `Data.Time.Calendar.diffDays` Data.Time.Clock.System.systemEpochDay
{-# INLINE date #-}

date32 :: Value Data.Time.Day
date32 = Value $ \date ->
  Data.ByteString.Builder.int32LE $!
    fromIntegral $
      date `Data.Time.Calendar.diffDays` Data.Time.Clock.System.systemEpochDay
{-# INLINE date32 #-}

encodeLEB128 :: Word64 -> Data.ByteString.Builder.Builder
encodeLEB128 = go
  where
    go !i
      | i <= 127 =
          Data.ByteString.Builder.word8 (fromIntegral i)
      | otherwise =
          -- bit 7 (8th bit) indicates more to come.
          let !byte = Data.Bits.setBit (fromIntegral i) 7
           in Data.ByteString.Builder.word8 byte <> go (i `Data.Bits.unsafeShiftR` 7)
