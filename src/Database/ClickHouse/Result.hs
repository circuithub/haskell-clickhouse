{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Database.ClickHouse.Result
  ( -- * Result
    Result,
    ClickhouseResultException (..),
    runResult,
    defaultFormat,
    noResult,
    singleRow,
    singleRowMaybe,
    manyRows,
    foldRows,

    -- * Row
    Row,
    column,

    -- * Column
    Column,
    nullable,
    int8,
    uint8,
    int16,
    uint16,
    int32,
    uint32,
    int64,
    uint64,
    string,
    float32,
    float64,
    bool,
    date,
    date32,
    dateTime,
    dateTime32,
    dateTime64,
    uuid,
    Database.ClickHouse.Result.map,
    array,

    -- * FixedString
    FixedString (..),
    fixedString,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad ((<$!>))
import Control.Monad.Catch (bracket)
import Control.Monad.IO.Class (liftIO)
-- import Data.Bits (unsafeShiftL, (.&.), (.|.))
import Data.ByteString qualified
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified
import Data.Hashable (Hashable)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
-- import Data.Text.Encoding qualified
import Data.Time (Day)
import Data.Time qualified
import Data.Time.Calendar qualified
import Data.Time.Clock.POSIX qualified
import Data.Time.Clock.System qualified
import Data.UUID qualified
import Data.Vector qualified
import Data.Vector.Generic qualified
import Data.Vector.Generic.Mutable qualified
import Data.Vector.Mutable qualified
import Data.Vector.Storable qualified
import Data.Vector.Unboxed qualified
import Data.Word (Word16, Word32, Word64, Word8)
import Database.ClickHouse.Internal.Parser qualified
import Database.ClickHouse.Stream qualified
import GHC.TypeLits qualified
import Network.HTTP.Client qualified

-- import qualified Data.Vector

-- | 'ClickhouseResultException' is thrown when a response parsing error occurs.
data ClickhouseResultException
  = -- | A row returned from Clickhouse is not a valid JSON array according to the
    -- JSONCompactEachRow format.
    QueryError String
  | -- | Type error when parsing a row.
    RowParseError
      -- | Error message
      String
  | -- | The response didn't contain any results but we expected exactly one
    EmptyResult
  | -- | The response did contain more than a single row.
    UnexpectedResult
  deriving stock (Show)
  deriving anyclass (Exception)

-- | This is the value that we expect to be set in the X-ClickHouse-Format
-- for everything to work properly.
defaultFormat :: Data.ByteString.ByteString
defaultFormat =
  "RowBinary"

-- | @`Row a` deserializes an @a@ from a ClickHouse query result.
data Row a = Row !Int (Database.ClickHouse.Internal.Parser.Parser a)
  deriving stock (Functor)

instance Applicative Row where
  pure x =
    Row 0 (pure x)
  {-# INLINE pure #-}

  Row x getf <*> Row y getx =
    Row (x + y) (getf <*> getx)
  {-# INLINE (<*>) #-}

-- | Lift a 'Column' decoder into a 'Row'.
--
-- @
-- 'column' 'uint64' :: 'Row' 'Data.Word.Word64'
-- @
column :: Column a -> Row a
column (Column get) = Row 1 get

-- | Decode a @Nullable(a)@ column.
--
-- @
-- 'column' ('nullable' 'string') :: 'Row' ('Maybe' 'Data.Text.Text')
-- @
nullable :: Column a -> Column (Maybe a)
nullable (Column get) = Column $ do
  w <- Database.ClickHouse.Internal.Parser.word8
  if w /= 0
    then pure Nothing
    else fmap Just get

newtype Column a = Column (Database.ClickHouse.Internal.Parser.Parser a)
  deriving newtype (Functor, Applicative)

-- | Decode a ClickHouse @Int8@ column.
int8 :: Column Int8
int8 =
  Column Database.ClickHouse.Internal.Parser.int8
{-# INLINE int8 #-}

-- | Decode a ClickHouse @UInt8@ column.
uint8 :: Column Word8
uint8 =
  Column Database.ClickHouse.Internal.Parser.word8
{-# INLINE uint8 #-}

-- | Decode a ClickHouse @Int16@ column.
int16 :: Column Int16
int16 =
  Column Database.ClickHouse.Internal.Parser.int16le
{-# INLINE int16 #-}

-- | Decode a ClickHouse @UInt16@ column.
uint16 :: Column Word16
uint16 =
  Column Database.ClickHouse.Internal.Parser.word16le
{-# INLINE uint16 #-}

-- | Decode a ClickHouse @Int32@ column.
int32 :: Column Int32
int32 =
  Column Database.ClickHouse.Internal.Parser.int32le
{-# INLINE int32 #-}

-- | Decode a ClickHouse @UInt32@ column.
uint32 :: Column Word32
uint32 =
  Column Database.ClickHouse.Internal.Parser.word32le
{-# INLINE uint32 #-}

-- | Decode a ClickHouse @Int64@ column.
int64 :: Column Int64
int64 =
  Column Database.ClickHouse.Internal.Parser.int64le
{-# INLINE int64 #-}

-- | Decode a ClickHouse @UInt64@ column.
uint64 :: Column Word64
uint64 =
  Column Database.ClickHouse.Internal.Parser.word64le
{-# INLINE uint64 #-}

-- | Decode a ClickHouse @String@ column.
string :: Column Text
string = Column $ do
  len <- Database.ClickHouse.Internal.Parser.uLEB128
  Database.ClickHouse.Internal.Parser.text (fromIntegral len)
{-# INLINE string #-}

newtype FixedString (n :: GHC.TypeLits.Nat) = FixedString Data.ByteString.ByteString

fixedString :: forall (n :: GHC.TypeLits.Nat). (GHC.TypeLits.KnownNat n) => Column (FixedString n)
fixedString =
  Column $
    fmap FixedString $
      Database.ClickHouse.Internal.Parser.byteString $
        fromIntegral (GHC.TypeLits.natVal (Proxy :: Proxy n))
{-# INLINE fixedString #-}

-- | Decode a ClickHouse @Date@ column.
date :: Column Day
date =
  Column $
    -- Data.Time.Clock.System.systemEpochDay is the day of the epoch of SystemTime, 1970-01-01
    (\days -> fromIntegral days `Data.Time.Calendar.addDays` Data.Time.Clock.System.systemEpochDay)
      <$!> Database.ClickHouse.Internal.Parser.int16le
{-# INLINE date #-}

-- | Decode a ClickHouse @Date32@ column.
date32 :: Column Day
date32 =
  Column $
    -- Data.Time.Clock.System.systemEpochDay is the day of the epoch of SystemTime, 1970-01-01
    (\days -> fromIntegral days `Data.Time.Calendar.addDays` Data.Time.Clock.System.systemEpochDay)
      <$!> Database.ClickHouse.Internal.Parser.int32le
{-# INLINE date32 #-}

-- | Decode a ClickHouse @Float32@ column.
float32 :: Column Float
float32 =
  Column Database.ClickHouse.Internal.Parser.float32le
{-# INLINE float32 #-}

-- | Decode a ClickHouse @Float64@ column.
float64 :: Column Double
float64 =
  Column Database.ClickHouse.Internal.Parser.float64le
{-# INLINE float64 #-}

-- | Decode a ClickHouse @Bool@ column.
bool :: Column Bool
bool =
  Column $
    (\x -> x > 0)
      <$!> Database.ClickHouse.Internal.Parser.word8
{-# INLINE bool #-}

-- | Decode a ClickHouse @UUID@ column.
uuid :: Column Data.UUID.UUID
uuid =
  Column $
    Data.UUID.fromWords64
      <$> Database.ClickHouse.Internal.Parser.word64le
      <*> Database.ClickHouse.Internal.Parser.word64le
{-# INLINE uuid #-}

-- | Decode a ClickHouse @DateTime@ column. Alias for 'dateTime32'.
dateTime :: Column Data.Time.UTCTime
dateTime = dateTime32
{-# INLINE dateTime #-}

-- | Decode a ClickHouse @DateTime@ column (second precision).
dateTime32 :: Column Data.Time.UTCTime
dateTime32 =
  Column $ do
    !time <- Database.ClickHouse.Internal.Parser.int32le
    let utcTime@Data.Time.UTCTime {utctDayTime = !_x, utctDay = !_y} =
          Data.Time.Clock.POSIX.posixSecondsToUTCTime (fromIntegral time)
    pure utcTime
{-# INLINE dateTime32 #-}

-- | Decode a ClickHouse @DateTime64(3)@ column (millisecond precision).
dateTime64 :: Column Data.Time.UTCTime
dateTime64 = Column $ do
  (\time -> Data.Time.Clock.POSIX.posixSecondsToUTCTime (fromIntegral time / 1000))
    <$!> Database.ClickHouse.Internal.Parser.int64le
{-# INLINE dateTime64 #-}

-- | Decode a ClickHouse @Map(k, v)@ column.
--
-- @
-- 'column' ('map' 'string' 'uint32') :: 'Row' ('HashMap' 'Data.Text.Text' 'Data.Word.Word32')
-- @
map :: (Hashable a) => Column a -> Column b -> Column (HashMap a b)
map (Column getKey) (Column getValue) = Column $ do
  len <- Database.ClickHouse.Internal.Parser.uLEB128
  entries len mempty
  where
    entries 0 !acc =
      pure acc
    entries !n !acc = do
      key <- getKey
      value <- getValue
      entries (n - 1) (Data.HashMap.Strict.insert key value acc)
{-# INLINE map #-}

-- | Decode a ClickHouse @Array(a)@ column.
--
-- @
-- 'column' ('array' 'uint32') :: 'Row' ('Data.Vector.Vector' 'Data.Word.Word32')
-- @
array :: (Data.Vector.Generic.Vector v a) => Column a -> Column (v a)
array (Column elem) = Column $ do
  len <-
    Database.ClickHouse.Internal.Parser.uLEB128
  xs <-
    liftIO $ Data.Vector.Generic.Mutable.new (fromIntegral len)
  go xs (fromIntegral len) 0
  where
    go !xs !n !i
      | i < n = do
          !x <- elem
          liftIO $ Data.Vector.Generic.Mutable.unsafeWrite xs i x
          go xs n (i + 1)
      | otherwise =
          liftIO $ Data.Vector.Generic.unsafeFreeze xs
{-# INLINEABLE array #-}
{-# SPECIALIZE array :: Column a -> Column (Data.Vector.Vector a) #-}
{-# SPECIALIZE array :: (Data.Vector.Storable.Storable a) => Column a -> Column (Data.Vector.Storable.Vector a) #-}
{-# SPECIALIZE array :: (Data.Vector.Unboxed.Unbox a) => Column a -> Column (Data.Vector.Unboxed.Vector a) #-}

-- | Describes how to deserialize the response of a ClickHouse query into a
-- value of type @a@.
--
-- Use 'noResult', 'singleRow', 'singleRowMaybe', or 'manyRows' to construct
-- a 'Result'.
newtype Result a = Result
  { runResult ::
      -- Make the request to get to a response. This is passed explicitly
      -- so that a 'Result' can decide itself what its lifecycle should be
      IO (Network.HTTP.Client.Response Network.HTTP.Client.BodyReader) ->
      IO a
  }

instance Functor Result where
  fmap f (Result run) = Result $ \request ->
    fmap f (run request)

-- | Discard the query response. Use this for statements that don't return rows
-- (e.g. @CREATE TABLE@, @DROP TABLE@).
--
-- @
-- 'Database.ClickHouse.runQuery' connection \"CREATE TABLE ...\" 'mempty' 'noResult' ()
-- @
noResult :: Result ()
noResult = Result $ \getResponse -> do
  bracket (liftIO getResponse) (liftIO . Network.HTTP.Client.responseClose) $ \_response ->
    pure ()

-- | Expect exactly one row in the response. Throws 'EmptyResult' if no rows
-- are returned, or 'UnexpectedResult' if more than one row is returned.
--
-- @
-- 'singleRow' ('column' 'uint64') :: 'Result' 'Data.Word.Word64'
-- @
singleRow :: Row a -> Result a
singleRow row = Result $ \getResponse -> do
  bracket (liftIO getResponse) (liftIO . Network.HTTP.Client.responseClose) $ \response -> do
    runDecoder
      (Network.HTTP.Client.responseBody response)
      row
      ( Fold
          (pure Nothing)
          ( \a state ->
              case state of
                Just {} -> throwIO UnexpectedResult
                Nothing -> pure $! Just $! a
          )
          ( \x -> case x of
              Just x -> pure x
              Nothing -> throwIO EmptyResult
          )
      )
{-# INLINE singleRow #-}

-- | Expect zero or one rows in the response. Returns 'Nothing' when the
-- response is empty. Throws 'UnexpectedResult' if more than one row is
-- returned.
--
-- @
-- 'singleRowMaybe' ('column' 'string') :: 'Result' ('Maybe' 'Data.Text.Text')
-- @
singleRowMaybe :: Row a -> Result (Maybe a)
singleRowMaybe row = Result $ \getResponse ->
  bracket (liftIO getResponse) (liftIO . Network.HTTP.Client.responseClose) $ \response -> do
    runDecoder
      (Network.HTTP.Client.responseBody response)
      row
      ( Fold
          (pure Nothing)
          ( \a state ->
              case state of
                Just {} -> throwIO UnexpectedResult
                Nothing -> pure $! Just $! a
          )
          pure
      )
{-# INLINE singleRowMaybe #-}

data Growable v a = Growable !Int !(v a)

-- | Collect all rows from the response into a 'Data.Vector.Vector'.
--
-- @
-- 'manyRows' ('column' 'string') :: 'Result' ('Data.Vector.Vector' 'Data.Text.Text')
-- @
--
-- Or with multiple columns:
--
-- @
-- 'manyRows' ((,) '<$>' 'column' 'string' '<*>' 'column' 'uint32')
--   :: 'Result' ('Data.Vector.Vector' ('Data.Text.Text', 'Data.Word.Word32'))
-- @
manyRows :: Row a -> Result (Data.Vector.Vector a)
manyRows row = Result $ \getResponse ->
  bracket (liftIO getResponse) (liftIO . Network.HTTP.Client.responseClose) $ \response -> do
    runDecoder
      (Network.HTTP.Client.responseBody response)
      row
      ( Fold
          ( Growable 0 <$> Data.Vector.Mutable.unsafeNew 0
          )
          ( \ !x (Growable n v) -> do
              v <-
                if n >= Data.Vector.Mutable.length v
                  then Data.Vector.Mutable.unsafeGrow v (max 1 n)
                  else pure v
              Data.Vector.Mutable.unsafeWrite v n x
              pure (Growable (n + 1) v)
          )
          ( \(Growable n v) ->
              Data.Vector.unsafeTake n <$> Data.Vector.unsafeFreeze v
          )
      )
{-# INLINE manyRows #-}

foldRows ::
  (a -> b -> a) ->
  a ->
  Row b ->
  Result a
foldRows step initial row = Result $ \getResponse ->
  bracket (liftIO getResponse) (liftIO . Network.HTTP.Client.responseClose) $ \response -> do
    runDecoder
      (Network.HTTP.Client.responseBody response)
      row
      ( Fold
          (pure $! initial)
          (\a state -> pure $! step state a)
          pure
      )
{-# INLINE foldRows #-}

data Fold input output where
  Fold ::
    IO state ->
    (input -> state -> IO state) ->
    (state -> IO output) ->
    Fold input output

runDecoder :: IO Data.ByteString.ByteString -> Row a -> Fold a b -> IO b
runDecoder source (Row _ parser) (Fold start step stop) = do
  state <- start
  let stream = Database.ClickHouse.Internal.Parser.parseFromSource source parser
  Database.ClickHouse.Stream.foldStream
    ( \state element ->
        case element of
          Right x -> step x state
          Left err -> throwIO (RowParseError err)
    )
    state
    stream
    >>= stop
{-# INLINE runDecoder #-}
