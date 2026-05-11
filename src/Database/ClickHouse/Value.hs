{-# LANGUAGE DataKinds #-}

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
    byteString,
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
    fixedString,
    tuple,
    tuple3,
    tuple4,
    tuple5,
    tuple6,
    tuple7,
  )
where

import Data.Bits qualified
import Data.ByteString.Builder qualified
import Data.ByteString.Char8 qualified as BS8
import Data.Functor.Contravariant (Contravariant (..))
import Data.Functor.Contravariant.Divisible (Decidable (..), Divisible (..))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import Data.Text.Encoding qualified
import Data.Text.Foreign qualified
import Data.Time qualified
import Data.Time.Calendar qualified
import Data.Time.Clock.POSIX qualified
import Data.Time.Clock.System qualified
import Data.UUID qualified
import Data.Void (absurd)
import Data.Word (Word16, Word32, Word64, Word8)
import Database.ClickHouse.Result (FixedString (..))
import GHC.Exts qualified
import GHC.TypeLits qualified

-- | An encoder that serializes a value of type @a@ for insertion into
-- ClickHouse.
--
-- 'Value' is 'Contravariant' and 'Semigroup' — adapt encoders to different
-- input types with 'contramap', and combine them sequentially with '<>' to
-- build row encoders.
--
-- === Single-column encoder
--
-- @
-- 'uint32' :: 'Value' 'Word32'
-- @
--
-- === Multi-column row encoder
--
-- @
-- 'Data.Functor.Contravariant.contramap' fst 'int64'
--   '<>' 'Data.Functor.Contravariant.contramap' snd 'string'
--   :: 'Value' ('Data.Int.Int64', 'Data.Text.Text')
-- @
--
-- === Nullable column
--
-- @
-- 'nullable' 'uint32' :: 'Value' ('Maybe' 'Word32')
-- @
newtype Value a = Value (a -> Data.ByteString.Builder.Builder)
  deriving newtype (Semigroup)

instance Contravariant Value where
  contramap f (Value g) =
    Value (\x -> g (f x))

instance Divisible Value where
  divide f (Value g) (Value h) = Value $ \a ->
    case f a of
      (b, c) -> g b <> h c
  conquer = Value $ \_ -> mempty

instance Decidable Value where
  lose f = Value $ \a -> absurd (f a)
  choose f (Value g) (Value h) = Value $ \a ->
    case f a of
      Left b -> g b
      Right c -> h c

runValue :: Value a -> a -> Data.ByteString.Builder.Builder
runValue (Value f) x = f x

-- | Encode an 'Int8'. Corresponds to ClickHouse @Int8@.
int8 :: Value Int8
int8 = Value Data.ByteString.Builder.int8
{-# INLINE int8 #-}

-- | Encode a 'Word8'. Corresponds to ClickHouse @UInt8@.
uint8 :: Value Word8
uint8 = Value Data.ByteString.Builder.word8
{-# INLINE uint8 #-}

-- | Encode an 'Int16'. Corresponds to ClickHouse @Int16@.
int16 :: Value Int16
int16 = Value Data.ByteString.Builder.int16LE
{-# INLINE int16 #-}

-- | Encode a 'Word16'. Corresponds to ClickHouse @UInt16@.
uint16 :: Value Word16
uint16 = Value Data.ByteString.Builder.word16LE
{-# INLINE uint16 #-}

-- | Encode an 'Int32'. Corresponds to ClickHouse @Int32@.
int32 :: Value Int32
int32 = Value Data.ByteString.Builder.int32LE
{-# INLINE int32 #-}

-- | Encode a 'Word32'. Corresponds to ClickHouse @UInt32@.
uint32 :: Value Word32
uint32 = Value Data.ByteString.Builder.word32LE
{-# INLINE uint32 #-}

-- | Encode an 'Int64'. Corresponds to ClickHouse @Int64@.
int64 :: Value Int64
int64 = Value Data.ByteString.Builder.int64LE
{-# INLINE int64 #-}

-- | Encode a 'Word64'. Corresponds to ClickHouse @UInt64@.
uint64 :: Value Word64
uint64 = Value Data.ByteString.Builder.word64LE
{-# INLINE uint64 #-}

-- | Encode a 'Text'. Corresponds to ClickHouse @String@.
string :: Value Text
string = Value $ \text ->
  encodeLEB128 (fromIntegral (Data.Text.Foreign.lengthWord8 text))
    <> Data.Text.Encoding.encodeUtf8Builder text
{-# INLINE string #-}

-- | Encode a 'ByteString'. Corresponds to ClickHouse @String@.
byteString :: Value BS8.ByteString
byteString = Value $ \bs ->
  encodeLEB128 (fromIntegral (BS8.length bs))
    <> Data.ByteString.Builder.byteString bs
{-# INLINE byteString #-}

-- | Encode a 'FixedString'. Corresponds to ClickHouse @FixedString(n)@.
--
-- The inner 'Data.ByteString.ByteString' is written as-is (no length prefix),
-- which matches the RowBinary wire format for @FixedString@.
--
-- Note: this encoder does /not/ check or enforce that the byte string has
-- exactly @n@ bytes, nor does it pad or truncate the value. It is the
-- caller's responsibility to ensure the length matches the column's
-- @FixedString(n)@ declaration.
fixedString :: forall (n :: GHC.TypeLits.Nat). Value (FixedString n)
fixedString = Value $ \(FixedString bs) ->
  Data.ByteString.Builder.byteString bs
{-# INLINE fixedString #-}

-- | Encode a 'Data.UUID.UUID'. Corresponds to ClickHouse @UUID@.
uuid :: Value Data.UUID.UUID
uuid = Value $ \uuid ->
  case Data.UUID.toWords64 uuid of
    (lo, hi) ->
      Data.ByteString.Builder.word64LE lo <> Data.ByteString.Builder.word64LE hi
{-# INLINE uuid #-}

-- | Encode a @Map(k, v)@.
--
-- @
-- 'map' 'string' 'uint32' :: 'Value' [('Data.Text.Text', 'Word32')]
-- @
map :: (GHC.Exts.IsList f, GHC.Exts.Item f ~ (a, b)) => Value a -> Value b -> Value f
map (Value f) (Value g) =
  contramap
    GHC.Exts.toList
    (array tuple2)
  where
    tuple2 = Value $ \(a, b) -> f a <> g b
{-# INLINE map #-}

-- | Encode an @Array(a)@.
--
-- @
-- 'array' 'uint32' :: 'Value' ['Word32']
-- @
array :: (Foldable f) => Value a -> Value (f a)
array (Value f) = Value $ \xs ->
  encodeLEB128 (fromIntegral (length xs)) <> foldMap f xs
{-# INLINE array #-}

-- | Encode a 'Float'. Corresponds to ClickHouse @Float32@.
float32 :: Value Float
float32 = Value Data.ByteString.Builder.floatLE
{-# INLINE float32 #-}

-- | Encode a 'Double'. Corresponds to ClickHouse @Float64@.
float64 :: Value Double
float64 = Value Data.ByteString.Builder.doubleLE
{-# INLINE float64 #-}

-- | Encode a 'Bool'. Corresponds to ClickHouse @Bool@.
bool :: Value Bool
bool = Value $ \x ->
  Data.ByteString.Builder.word8 (if x then 1 else 0)
{-# INLINE bool #-}

-- | Encode a @Nullable(a)@. 'Nothing' is encoded as null.
--
-- @
-- 'nullable' 'uint32' :: 'Value' ('Maybe' 'Word32')
-- @
nullable :: Value a -> Value (Maybe a)
nullable (Value f) = Value $ \x ->
  case x of
    Nothing -> Data.ByteString.Builder.word8 1
    Just x -> Data.ByteString.Builder.word8 0 <> f x
{-# INLINE nullable #-}

-- | Encode a 'Data.Time.UTCTime'. Alias for 'dateTime32'.
-- Corresponds to ClickHouse @DateTime@.
dateTime :: Value Data.Time.UTCTime
dateTime = dateTime32
{-# INLINE dateTime #-}

-- | Encode a 'Data.Time.UTCTime' with second precision.
-- Corresponds to ClickHouse @DateTime@.
dateTime32 :: Value Data.Time.UTCTime
dateTime32 = Value $ \time ->
  Data.ByteString.Builder.int32LE $!
    round (Data.Time.Clock.POSIX.utcTimeToPOSIXSeconds time)
{-# INLINE dateTime32 #-}

-- | Encode a 'Data.Time.UTCTime' with millisecond precision.
-- Corresponds to ClickHouse @DateTime64(3)@.
dateTime64 :: Value Data.Time.UTCTime
dateTime64 = Value $ \time ->
  Data.ByteString.Builder.int64LE $!
    round (Data.Time.Clock.POSIX.utcTimeToPOSIXSeconds time * 1000)
{-# INLINE dateTime64 #-}

-- | Encode a 'Data.Time.Day'. Corresponds to ClickHouse @Date@.
date :: Value Data.Time.Day
date = Value $ \date ->
  Data.ByteString.Builder.int16LE $!
    fromIntegral $
      date `Data.Time.Calendar.diffDays` Data.Time.Clock.System.systemEpochDay
{-# INLINE date #-}

-- | Encode a 'Data.Time.Day' with extended range. Corresponds to ClickHouse @Date32@.
date32 :: Value Data.Time.Day
date32 = Value $ \date ->
  Data.ByteString.Builder.int32LE $!
    fromIntegral $
      date `Data.Time.Calendar.diffDays` Data.Time.Clock.System.systemEpochDay
{-# INLINE date32 #-}

-- | Encode a @Tuple(a, b)@.
--
-- @
-- 'tuple' 'string' 'uint32' :: 'Value' ('Data.Text.Text', 'Word32')
-- @
tuple :: Value a -> Value b -> Value (a, b)
tuple (Value f) (Value g) = Value $ \(a, b) -> f a <> g b
{-# INLINE tuple #-}

-- | Encode a @Tuple(a, b, c)@.
tuple3 :: Value a -> Value b -> Value c -> Value (a, b, c)
tuple3 (Value f) (Value g) (Value h) = Value $ \(a, b, c) -> f a <> g b <> h c
{-# INLINE tuple3 #-}

-- | Encode a @Tuple(a, b, c, d)@.
tuple4 :: Value a -> Value b -> Value c -> Value d -> Value (a, b, c, d)
tuple4 (Value f) (Value g) (Value h) (Value i) = Value $ \(a, b, c, d) -> f a <> g b <> h c <> i d
{-# INLINE tuple4 #-}

-- | Encode a @Tuple(a, b, c, d, e)@.
tuple5 :: Value a -> Value b -> Value c -> Value d -> Value e -> Value (a, b, c, d, e)
tuple5 (Value f) (Value g) (Value h) (Value i) (Value j) = Value $ \(a, b, c, d, e) -> f a <> g b <> h c <> i d <> j e
{-# INLINE tuple5 #-}

-- | Encode a @Tuple(a, b, c, d, e, f)@.
tuple6 :: Value a -> Value b -> Value c -> Value d -> Value e -> Value f -> Value (a, b, c, d, e, f)
tuple6 (Value va) (Value vb) (Value vc) (Value vd) (Value ve) (Value vf) = Value $ \(a, b, c, d, e, f) -> va a <> vb b <> vc c <> vd d <> ve e <> vf f
{-# INLINE tuple6 #-}

-- | Encode a @Tuple(a, b, c, d, e, f, g)@.
tuple7 :: Value a -> Value b -> Value c -> Value d -> Value e -> Value f -> Value g -> Value (a, b, c, d, e, f, g)
tuple7 (Value va) (Value vb) (Value vc) (Value vd) (Value ve) (Value vf) (Value vg) = Value $ \(a, b, c, d, e, f, g) -> va a <> vb b <> vc c <> vd d <> ve e <> vf f <> vg g
{-# INLINE tuple7 #-}

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
