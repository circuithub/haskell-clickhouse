{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

module Database.ClickHouse.Internal.Parser
  ( Parser (..),
    ParseResult (..),
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
    checkBounds,
    parserAp,
    parserBind,
    parserFmap,
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
import Database.ClickHouse.Stream qualified
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
  fmap = parserFmap
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure x = Parser $ \_ pos -> return $ ParseSuccess pos x

  (<*>) = parserAp
  {-# INLINE (<*>) #-}

instance Monad Parser where
  return = pure

  (>>=) = parserBind
  {-# INLINE (>>=) #-}

-- | Apply a function to the result of a parser.
--
-- Named wrapper used in rewrite rules to fuse 'checkBounds' through 'fmap'.
parserFmap :: (a -> b) -> Parser a -> Parser b
parserFmap f (Parser p) = Parser $ \end pos -> do
  result <- p end pos
  case result of
    ParseSuccess pos' x -> return $ ParseSuccess pos' (f x)
    ParseFailure err -> return $ ParseFailure err
    UnexpectedEndOfInput -> return UnexpectedEndOfInput
{-# NOINLINE [1] parserFmap #-}

-- | Applicative sequencing for parsers.
--
-- Named wrapper used in rewrite rules to fuse consecutive 'checkBounds'.
parserAp :: Parser (a -> b) -> Parser a -> Parser b
parserAp (Parser pf) (Parser px) = Parser $ \end pos -> do
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
{-# NOINLINE [1] parserAp #-}

-- | Monadic bind for parsers.
--
-- Named wrapper used in rewrite rules to fuse consecutive 'checkBounds'.
parserBind :: Parser a -> (a -> Parser b) -> Parser b
parserBind (Parser pa) f = Parser $ \end pos -> do
  result <- pa end pos
  case result of
    ParseSuccess pos' x -> do
      let Parser pb = f x
      pb end pos'
    ParseFailure err -> return $ ParseFailure err
    UnexpectedEndOfInput -> return UnexpectedEndOfInput
{-# NOINLINE [1] parserBind #-}

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

parseFromSource :: IO ByteString -> Parser a -> Database.ClickHouse.Stream.Stream IO (Either String a)
parseFromSource source parser = go0
  where
    go0 = Database.ClickHouse.Stream.Stream $ \yield done -> do
      chunk <- source
      if BS.null chunk
        then done
        else Database.ClickHouse.Stream.unStream (go1 chunk) yield done

    go1 !acc = Database.ClickHouse.Stream.Stream $ \yield done ->
      case runParser parser acc of
        (ParseSuccess _ result, remaining)
          | BS.null remaining ->
              yield (Right result) go0
          | otherwise ->
              yield (Right result) (go1 remaining)
        (ParseFailure err, _) ->
          yield (Left err) Database.ClickHouse.Stream.empty
        (UnexpectedEndOfInput, _) -> do
          chunk <- source
          if BS.null chunk
            then yield (Left "unexpected end of input") Database.ClickHouse.Stream.empty
            else Database.ClickHouse.Stream.unStream (go1 (acc <> chunk)) yield done

checkBounds :: Int -> Parser a -> Parser a
checkBounds n (Parser k) = Parser $ \end pos ->
  if pos `plusPtr` n <= end
    then k end pos
    else return UnexpectedEndOfInput
{-# NOINLINE [1] checkBounds #-}

-- Rewrite rules that fuse consecutive checkBounds into a single check.
--
-- When two parsers each guarded by checkBounds are sequenced via parserAp,
-- the two bounds checks can be merged: if the first parser consumes exactly
-- n bytes and the second needs m bytes, checking (n + m) bytes upfront
-- suffices and eliminates the second check.
--
-- The rules are keyed on parserAp/parserBind/parserFmap (named functions
-- we control) rather than on class methods, which ensures they fire
-- reliably across module boundaries.
{-# RULES
"checkBounds/parserAp" forall n m p q.
  parserAp (checkBounds n p) (checkBounds m q) =
    checkBounds (n + m) (parserAp p q)
"checkBounds/parserBind" forall n m p f.
  parserBind (checkBounds n p) (\x -> checkBounds m (f x)) =
    checkBounds (n + m) (parserBind p f)
"checkBounds/parserFmap" forall f n p.
  parserFmap f (checkBounds n p) =
    checkBounds n (parserFmap f p)
  #-}

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
