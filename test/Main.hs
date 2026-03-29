{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Text.Lazy.Builder qualified as TBuilder
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Word (Word16, Word32, Word64)
import Database.ClickHouse.Insert qualified as Insert
import Database.ClickHouse.Params qualified as Params
import Database.ClickHouse.Parser qualified as Parser
import Database.ClickHouse.Value qualified as Value
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@=?))
import Test.Tasty.HUnit qualified as HUnit

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.testGroup
      "clickhouse-client"
      [ paramsTests,
        insertTests,
        valueRoundtripTests
      ]

paramsTests :: Tasty.TestTree
paramsTests =
  Tasty.testGroup
    "Params"
    [ HUnit.testCase "uint32 param is prefixed and encoded" $ do
        Params.runParam (Params.uint32 "x") (42 :: Word32)
          @=? [("param_x", Just "42")],
      HUnit.testCase "utcTime param uses ClickHouse-friendly format" $ do
        let t = UTCTime (fromGregorian 2024 6 15) (secondsToDiffTime 36000)
        Params.runParam (Params.utcTime "created_at") t
          @=? [("param_created_at", Just "2024-06-15T10:00:00")]
    ]

insertTests :: Tasty.TestTree
insertTests =
  Tasty.testGroup
    "Insert"
    [ HUnit.testCase "renderInsert includes columns and settings" $ do
        let ins =
              Insert.modifySettings
                (("async_insert", "1") :)
                (Insert.insert "events" ["id", "name"] (Value.tuple Value.uint32 Value.string) mempty)
            rendered =
              Text.unpack . LText.toStrict . TBuilder.toLazyText $ Insert.renderInsert ins
        rendered
          @=? "INSERT INTO events (\"id\", \"name\") SETTINGS async_insert = 1 FORMAT RowBinary"
    ]

valueRoundtripTests :: Tasty.TestTree
valueRoundtripTests =
  Tasty.testGroup
    "Value encoding"
    [ roundtripCase "uint64" Value.uint64 Parser.word64le (123456789 :: Word64),
      roundtripCase "string" Value.string parseText "hello clickhouse",
      roundtripCase "nullable just" (Value.nullable Value.uint32) parseNullableWord32 (Just 7),
      roundtripCase "nullable nothing" (Value.nullable Value.uint32) parseNullableWord32 Nothing,
      roundtripCase "array uint16" (Value.array Value.uint16) parseWord16Array [1, 2, 3, 65535],
      roundtripCase
        "map string->uint32"
        (Value.map Value.string Value.uint32)
        parseStringUInt32Map
        (HashMap.fromList [("a", 1), ("bbb", 42)]),
      roundtripCase
        "tuple string uint32"
        (Value.tuple Value.string Value.uint32)
        ((,) <$> parseText <*> Parser.word32le)
        ("event", 99)
    ]

roundtripCase :: (Eq a, Show a) => String -> Value.Value a -> Parser.Parser a -> a -> Tasty.TestTree
roundtripCase label encoder parser expected =
  HUnit.testCase label $ do
    let bs = encode encoder expected
        (result, remaining) = Parser.runParser parser bs
    "" @=? remaining
    case result of
      Parser.ParseSuccess _ actual -> expected @=? actual
      Parser.ParseFailure err -> HUnit.assertFailure err
      Parser.UnexpectedEndOfInput -> HUnit.assertFailure "unexpected end of input"

encode :: Value.Value a -> a -> ByteString
encode encoder = LBS.toStrict . Builder.toLazyByteString . Value.runValue encoder

parseText :: Parser.Parser Text.Text
parseText = do
  len <- Parser.uLEB128
  Parser.text (fromIntegral len)

parseNullableWord32 :: Parser.Parser (Maybe Word32)
parseNullableWord32 = do
  tag <- Parser.word8
  case tag of
    0 -> Just <$> Parser.word32le
    1 -> pure Nothing
    _ -> failParser "invalid nullable tag"

parseWord16Array :: Parser.Parser [Word16]
parseWord16Array = do
  len <- Parser.uLEB128
  let n = fromIntegral len :: Int
  sequence (replicate n Parser.word16le)

parseStringUInt32Map :: Parser.Parser (HashMap.HashMap Text.Text Word32)
parseStringUInt32Map = do
  len <- Parser.uLEB128
  let n = fromIntegral len :: Int
  pairs <- sequence (replicate n ((,) <$> parseText <*> Parser.word32le))
  pure (HashMap.fromList pairs)

failParser :: String -> Parser.Parser a
failParser msg = Parser.Parser $ \_ _ -> pure (Parser.ParseFailure msg)
