{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

module Database.ClickHouse.Parser
  ( Parser (..),
    ParseResult (..),
    Stream (..),
    runParser,
    parseFromSource,
    word8,
    word16le,
    word32le,
    word64le,
    int8,
    int16le,
    int32le,
    int64le,
    float32le,
    float64le,
    byteString,
    text,
    takeN,
    uLEB128,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int
import Data.Text (Text)
import Data.Text.Foreign qualified
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import GHC.ByteOrder (ByteOrder (..), targetByteOrder)
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import System.IO.Unsafe (unsafeDupablePerformIO)

data ParseResult a
  = ParseSuccess !(Ptr Word8) a
  | ParseFailure String
  | UnexpectedEndOfInput
  deriving (Show, Eq)

newtype Parser a = Parser
  { unParser ::
      Ptr Word8 -> -- End position of the input
      Ptr Word8 -> -- Current position in the input
      IO (ParseResult a)
  }

instance MonadIO Parser where
  liftIO action = Parser $ \_ pos -> do
    result <- action
    pure (ParseSuccess pos result)

instance Functor Parser where
  fmap f (Parser p) = Parser $ \end pos -> do
    result <- p end pos
    case result of
      ParseSuccess pos' x -> return $ ParseSuccess pos' (f x)
      ParseFailure err -> return $ ParseFailure err
      UnexpectedEndOfInput -> return UnexpectedEndOfInput

instance Applicative Parser where
  pure x = Parser $ \_ pos -> return $ ParseSuccess pos x

  Parser pf <*> Parser px = Parser $ \end pos -> do
    result <- pf end pos
    case result of
      ParseSuccess pos' f -> do
        result' <- px end pos'
        case result' of
          ParseSuccess pos'' x -> return $ ParseSuccess pos'' (f x)
          ParseFailure err -> return $ ParseFailure err
          UnexpectedEndOfInput -> return UnexpectedEndOfInput
      ParseFailure err -> return $ ParseFailure err
      UnexpectedEndOfInput -> return UnexpectedEndOfInput

instance Monad Parser where
  return = pure

  Parser px >>= f = Parser $ \end pos -> do
    result <- px end pos
    case result of
      ParseSuccess pos' x -> do
        let Parser py = f x
        py end pos'
      ParseFailure err -> return $ ParseFailure err
      UnexpectedEndOfInput -> return UnexpectedEndOfInput

runParser :: Parser a -> ByteString -> (ParseResult a, ByteString)
runParser (Parser p) bs = unsafeDupablePerformIO $ do
  BSU.unsafeUseAsCStringLen bs $ \(ptr, len) -> do
    let !start = castPtr ptr
        !end = start `plusPtr` len
    result <- p end start
    case result of
      ParseSuccess pos' x -> do
        let !consumed = pos' `minusPtr` start
            !remaining = BS.drop consumed bs
        return (ParseSuccess pos' x, remaining)
      ParseFailure err -> return (ParseFailure err, bs)
      UnexpectedEndOfInput -> return (UnexpectedEndOfInput, bs)

newtype Stream a = Stream (IO (Maybe (a, Stream a)))

parseFromSource :: IO ByteString -> Parser a -> Stream (Either String a)
parseFromSource source parser = Stream go0
  where
    go0 = do
      chunk <- source
      if BS.null chunk
        then pure Nothing
        else go1 chunk

    go1 !acc = do
      case runParser parser acc of
        (ParseSuccess _ result, remaining)
          | BS.null remaining ->
              return $ Just (Right result, Stream go0)
          | otherwise ->
              return $ Just (Right result, Stream (go1 remaining))
        (ParseFailure err, _) ->
          return $ Just (Left err, Stream (return Nothing))
        (UnexpectedEndOfInput, _) -> do
          chunk <- source
          if BS.null chunk
            then return $ Just (Left "unexpected end of input", Stream (return Nothing))
            else go1 (acc <> chunk)

checkBounds :: Int -> Parser a -> Parser a
checkBounds n (Parser k) = Parser $ \end pos ->
  if pos `plusPtr` n <= end
    then k end pos
    else return UnexpectedEndOfInput

{-# INLINE word8 #-}
word8 :: Parser Word8
word8 = Parser $ \end pos ->
  if pos < end
    then do
      w <- peek pos
      return $! ParseSuccess (pos `plusPtr` 1) w
    else return UnexpectedEndOfInput

{-# INLINE int8 #-}
int8 :: Parser Int8
int8 = fromIntegral <$> word8

{-# INLINE word16le #-}
word16le :: Parser Word16
word16le = checkBounds 2 $ Parser $ \_ pos -> do
  w <- peek (castPtr pos)
  let !result = case targetByteOrder of
        LittleEndian -> w
        BigEndian -> byteSwap16 w
  return $ ParseSuccess (pos `plusPtr` 2) result

{-# INLINE word32le #-}
word32le :: Parser Word32
word32le = checkBounds 4 $ Parser $ \_ pos -> do
  w <- peek (castPtr pos)
  let !result = case targetByteOrder of
        LittleEndian -> w
        BigEndian -> byteSwap32 w
  return $ ParseSuccess (pos `plusPtr` 4) result

{-# INLINE word64le #-}
word64le :: Parser Word64
word64le = checkBounds 8 $ Parser $ \_ pos -> do
  w <- peek (castPtr pos)
  let !result = case targetByteOrder of
        LittleEndian -> w
        BigEndian -> byteSwap64 w
  return $ ParseSuccess (pos `plusPtr` 8) result

{-# INLINE int16le #-}
int16le :: Parser Int16
int16le = fromIntegral <$> word16le

{-# INLINE int32le #-}
int32le :: Parser Int32
int32le = fromIntegral <$> word32le

{-# INLINE int64le #-}
int64le :: Parser Int64
int64le = fromIntegral <$> word64le

{-# INLINE float32le #-}
float32le :: Parser Float
float32le = castWord32ToFloat <$> word32le

{-# INLINE float64le #-}
float64le :: Parser Double
float64le = castWord64ToDouble <$> word64le

{-# INLINE takeN #-}
takeN :: Int -> Parser ByteString
takeN n = checkBounds n $ Parser $ \_ pos -> do
  bs <- BS.packCStringLen (castPtr pos, n)
  return $ ParseSuccess (pos `plusPtr` n) bs

{-# INLINE byteString #-}
byteString :: Int -> Parser ByteString
byteString = takeN

{-# INLINE text #-}
text ::
  -- | Length in bytes
  Int ->
  Parser Text
text n =
  checkBounds n $ Parser $ \_ pos -> do
    !text <- Data.Text.Foreign.peekCStringLen (castPtr pos, n)
    return $ ParseSuccess (pos `plusPtr` n) text

{-# INLINE uLEB128 #-}
uLEB128 :: Parser Word64
uLEB128 = Parser $ \end pos ->
  let go !ptr !result !shift
        | ptr >= end = return UnexpectedEndOfInput
        | shift >= 64 = return $ ParseFailure "uLEB128: value too large"
        | otherwise = do
            byte <- peek ptr
            let !value = fromIntegral (byte .&. 0x7F)
                !newResult = result .|. (value `shiftL` shift)
                !newPtr = ptr `plusPtr` 1
            if byte .&. 0x80 == 0
              then return $ ParseSuccess newPtr newResult
              else go newPtr newResult (shift + 7)
   in go pos 0 0
