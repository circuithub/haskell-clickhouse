module Main (main) where

import Data.Foldable (toList)
import Data.Functor.Contravariant (contramap, ($<))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Text (Text)
import Data.Text qualified
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Data.Typeable (Typeable)
import Data.Typeable qualified
import Data.UUID (UUID)
import Data.UUID qualified
import Data.Vector qualified
import Data.Word (Word16, Word32, Word64, Word8)
import Database.ClickHouse qualified
import Database.ClickHouse.Insert qualified
import Database.ClickHouse.Params qualified
import Database.ClickHouse.Result qualified
import Database.ClickHouse.Value qualified
import Test.Tasty qualified
import Test.Tasty.HUnit (testCase, (@=?))
import TestContainers qualified
import TestContainers.Tasty qualified

main :: IO ()
main =
  Test.Tasty.defaultMainWithIngredients
    (TestContainers.Tasty.ingredient : Test.Tasty.defaultIngredients)
    (TestContainers.Tasty.withContainers setup tests)

setup :: TestContainers.TestContainer Database.ClickHouse.Connection
setup = do
  clickHouse <-
    TestContainers.run $
      TestContainers.setExpose [8123] $
        TestContainers.setWaitingFor (TestContainers.waitForHttp 8123 "/" [200]) $
          TestContainers.containerRequest $
            TestContainers.fromTag "clickhouse@sha256:114b90b999fbc52f96cc6500f11e4155a8265837da8554fc946a9849850479f6"

  let (host, port) = TestContainers.containerAddress clickHouse 8123

  Database.ClickHouse.newConnection
    ( Database.ClickHouse.ConnectionOptions
        { Database.ClickHouse.url = "http://" <> host <> ":" <> Data.Text.pack (show port),
          Database.ClickHouse.database = Nothing,
          Database.ClickHouse.user = Nothing,
          Database.ClickHouse.password = Nothing,
          Database.ClickHouse.httpManager = Nothing
        }
    )

tests :: IO Database.ClickHouse.Connection -> Test.Tasty.TestTree
tests newConnection =
  Test.Tasty.testGroup
    "ClickHouse"
    [ Test.Tasty.testGroup "Primitive data types" (primitives newConnection),
      Test.Tasty.testGroup "Nullable data types" (nullables newConnection),
      Test.Tasty.testGroup "Array data types" (arrays newConnection),
      Test.Tasty.testGroup "Map data types" (maps newConnection),
      Test.Tasty.testGroup "Value insert" (valueInserts newConnection),
      Test.Tasty.testGroup "Stress test roundtrip" (stressTests newConnection),
      Test.Tasty.testGroup "Wide table smoketest" (wideTableSmoketest newConnection)
    ]

data PrimitiveTestCase = forall a.
  (Eq a, Show a, Typeable a) =>
  PrimitiveTestCase
  { clickHouseType :: Text,
    clickHouseParam :: Text -> Database.ClickHouse.Params.Param a,
    clickHouseResult :: Database.ClickHouse.Result.Column a,
    expected :: a
  }

primitiveTestCases_integral :: (Integral a, Num a, Bounded a) => (a -> PrimitiveTestCase) -> [PrimitiveTestCase]
primitiveTestCases_integral mk =
  map mk [0, minBound, maxBound]

primitiveTestCases_string :: [PrimitiveTestCase]
primitiveTestCases_string =
  [ mk "",
    mk "hello world",
    mk "こんにちは",
    mk "😀😁😂🤣😜",
    mk "עברית שפה יפה"
  ]
  where
    mk expected =
      PrimitiveTestCase
        { clickHouseType = "String",
          clickHouseParam = Database.ClickHouse.Params.string,
          clickHouseResult = Database.ClickHouse.Result.string,
          expected
        }

primitiveTestCases_datetime :: [PrimitiveTestCase]
primitiveTestCases_datetime =
  [ mkD (read "2024-01-31"),
    mkD32 (read "2024-01-31"),
    mkDateTime (UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)),
    mkDateTime64 (UTCTime (fromGregorian 2024 6 15) (secondsToDiffTime 36000))
  ]
  where
    mkD32 expected =
      PrimitiveTestCase
        { clickHouseType = "Date32",
          clickHouseParam = Database.ClickHouse.Params.day,
          clickHouseResult = Database.ClickHouse.Result.date32,
          expected
        }

    mkD expected =
      PrimitiveTestCase
        { clickHouseType = "Date",
          clickHouseParam = Database.ClickHouse.Params.day,
          clickHouseResult = Database.ClickHouse.Result.date,
          expected
        }

    mkDateTime expected =
      PrimitiveTestCase
        { clickHouseType = "DateTime",
          clickHouseParam = Database.ClickHouse.Params.utcTime,
          clickHouseResult = Database.ClickHouse.Result.dateTime,
          expected
        }

    mkDateTime64 expected =
      PrimitiveTestCase
        { clickHouseType = "DateTime64(3)",
          clickHouseParam = Database.ClickHouse.Params.utcTime,
          clickHouseResult = Database.ClickHouse.Result.dateTime64,
          expected
        }

data SomeValue = forall a. (Eq a, Show a, Typeable a) => SomeValue a

instance Eq SomeValue where
  SomeValue a == SomeValue b =
    maybe False (== a) (Data.Typeable.cast b)

instance Show SomeValue where
  show (SomeValue x) = show x

primitives :: IO Database.ClickHouse.Connection -> [Test.Tasty.TestTree]
primitives newConnection =
  [ Test.Tasty.testGroup
      (show expected <> "::" <> Data.Text.unpack clickHouseType)
      [ --
        testCase "singleRow" $ do
          connection <- newConnection
          result <-
            Database.ClickHouse.runQuery
              connection
              (selectX clickHouseType)
              (clickHouseParam "x")
              (Database.ClickHouse.singleRow (Database.ClickHouse.Result.column clickHouseResult))
              expected
          expected @=? result,
        --
        testCase "singleRowMaybe" $ do
          connection <- newConnection
          result <-
            Database.ClickHouse.runQuery
              connection
              (selectX clickHouseType)
              (clickHouseParam "x")
              (Database.ClickHouse.singleRowMaybe (Database.ClickHouse.Result.column clickHouseResult))
              expected
          Just expected @=? result,
        --
        testCase "manyRows" $ do
          connection <- newConnection
          let n = 1000
          result <-
            Database.ClickHouse.runQuery
              connection
              (selectNX n clickHouseType)
              (clickHouseParam "x")
              ( Database.ClickHouse.manyRows
                  (Database.ClickHouse.Result.column clickHouseResult)
              )
              expected
          replicate n expected @=? toList result,
        --
        testCase "foldRows" $ do
          connection <- newConnection
          let n = 1000
          result <-
            Database.ClickHouse.runQuery
              connection
              (selectNX n clickHouseType)
              (clickHouseParam "x")
              ( Database.ClickHouse.Result.foldRows
                  (\xs x -> x : xs)
                  []
                  (Database.ClickHouse.Result.column clickHouseResult)
              )
              expected

          replicate n expected @=? reverse result
      ]
    | PrimitiveTestCase {..} <- testCases
  ]
    <> [ let query1 :: Text
             query1 =
               "SELECT "
                 <> Data.Text.intercalate
                   ", "
                   ( zipWith
                       ( \i PrimitiveTestCase {..} ->
                           "{ " <> Data.Text.pack ("param_" <> show (i :: Int)) <> " : " <> clickHouseType <> " }"
                       )
                       [1 ..]
                       testCases
                   )

             queryN :: Int -> Text
             queryN n =
               "SELECT * FROM VALUES("
                 <> Data.Text.intercalate
                   ", "
                   ( replicate
                       n
                       ( "("
                           <> Data.Text.intercalate
                             ", "
                             ( zipWith
                                 ( \i PrimitiveTestCase {..} ->
                                     "{ " <> Data.Text.pack ("param_" <> show (i :: Int)) <> " : " <> clickHouseType <> " }"
                                 )
                                 [1 ..]
                                 testCases
                             )
                           <> ")"
                       )
                   )
                 <> ")"

             params :: Database.ClickHouse.Params.Param ()
             params =
               mconcat $
                 zipWith
                   ( \i PrimitiveTestCase {..} ->
                       clickHouseParam (Data.Text.pack ("param_" <> show (i :: Int))) $< expected
                   )
                   [1 ..]
                   testCases

             columns :: Database.ClickHouse.Result.Row [SomeValue]
             columns =
               sequenceA
                 [ SomeValue <$> Database.ClickHouse.Result.column clickHouseResult
                   | PrimitiveTestCase {..} <- testCases
                 ]

             expecteds :: [SomeValue]
             expecteds =
               [SomeValue expected | PrimitiveTestCase {..} <- testCases]
          in Test.Tasty.testGroup
               "Multi column tests"
               [ --
                 testCase "singleRow" $ do
                   connection <- newConnection
                   result <-
                     Database.ClickHouse.runQuery
                       connection
                       query1
                       params
                       (Database.ClickHouse.singleRow columns)
                       ()
                   expecteds @=? result,
                 --
                 testCase "manyRows" $ do
                   connection <- newConnection
                   let n = 100
                   result <-
                     Database.ClickHouse.runQuery
                       connection
                       (queryN n)
                       params
                       (Database.ClickHouse.manyRows columns)
                       ()
                   replicate n expecteds @=? toList result,
                 --
                 testCase "foldRows" $ do
                   connection <- newConnection
                   let n = 100
                   result <-
                     Database.ClickHouse.runQuery
                       connection
                       (queryN n)
                       params
                       ( Database.ClickHouse.Result.foldRows
                           (\xs x -> x : xs)
                           []
                           columns
                       )
                       ()

                   replicate n expecteds @=? reverse result
               ]
       ]
  where
    testCases =
      concat
        [ primitiveTestCases_string,
          primitiveTestCases_datetime,
          primitiveTestCases_integral $
            PrimitiveTestCase "UInt8" Database.ClickHouse.Params.uint8 Database.ClickHouse.Result.uint8,
          primitiveTestCases_integral $
            PrimitiveTestCase "UInt16" Database.ClickHouse.Params.uint16 Database.ClickHouse.Result.uint16,
          primitiveTestCases_integral $
            PrimitiveTestCase "UInt32" Database.ClickHouse.Params.uint32 Database.ClickHouse.Result.uint32,
          primitiveTestCases_integral $
            PrimitiveTestCase "UInt64" Database.ClickHouse.Params.uint64 Database.ClickHouse.Result.uint64,
          primitiveTestCases_integral $
            PrimitiveTestCase "Int8" Database.ClickHouse.Params.int8 Database.ClickHouse.Result.int8,
          primitiveTestCases_integral $
            PrimitiveTestCase "Int16" Database.ClickHouse.Params.int16 Database.ClickHouse.Result.int16,
          primitiveTestCases_integral $
            PrimitiveTestCase "Int32" Database.ClickHouse.Params.int32 Database.ClickHouse.Result.int32,
          primitiveTestCases_integral $
            PrimitiveTestCase "Int64" Database.ClickHouse.Params.int64 Database.ClickHouse.Result.int64
        ]

    selectX type_ =
      "SELECT { x : " <> type_ <> "}"

    selectNX n type_ =
      "SELECT arrayJoin([" <> Data.Text.intercalate "," (replicate n ("{ x : " <> type_ <> "}")) <> "])"

-- ---------------------------------------------------------------------------
-- Nullable tests
-- ---------------------------------------------------------------------------

data NullableTestCase = forall a.
  (Eq a, Show a, Typeable a) =>
  NullableTestCase
  { nullableType :: Text,
    nullableParam :: Text -> Database.ClickHouse.Params.Param a,
    nullableResult :: Database.ClickHouse.Result.Column (Maybe a),
    nullableExpected :: Maybe a
  }

nullableTestCases :: [NullableTestCase]
nullableTestCases =
  concat
    [ -- Nullable strings: Just values and Nothing
      [ NullableTestCase
          { nullableType = "Nullable(String)",
            nullableParam = Database.ClickHouse.Params.string,
            nullableResult = Database.ClickHouse.Result.nullable Database.ClickHouse.Result.string,
            nullableExpected = Just "hello"
          },
        NullableTestCase
          { nullableType = "Nullable(String)",
            nullableParam = Database.ClickHouse.Params.string,
            nullableResult = Database.ClickHouse.Result.nullable Database.ClickHouse.Result.string,
            nullableExpected = Just ""
          }
      ],
      -- Nullable integers: Just values
      [ NullableTestCase
          { nullableType = "Nullable(UInt8)",
            nullableParam = Database.ClickHouse.Params.uint8,
            nullableResult = Database.ClickHouse.Result.nullable Database.ClickHouse.Result.uint8,
            nullableExpected = Just 42
          },
        NullableTestCase
          { nullableType = "Nullable(UInt16)",
            nullableParam = Database.ClickHouse.Params.uint16,
            nullableResult = Database.ClickHouse.Result.nullable Database.ClickHouse.Result.uint16,
            nullableExpected = Just 1000
          },
        NullableTestCase
          { nullableType = "Nullable(UInt32)",
            nullableParam = Database.ClickHouse.Params.uint32,
            nullableResult = Database.ClickHouse.Result.nullable Database.ClickHouse.Result.uint32,
            nullableExpected = Just 100000
          },
        NullableTestCase
          { nullableType = "Nullable(UInt64)",
            nullableParam = Database.ClickHouse.Params.uint64,
            nullableResult = Database.ClickHouse.Result.nullable Database.ClickHouse.Result.uint64,
            nullableExpected = Just 1000000
          },
        NullableTestCase
          { nullableType = "Nullable(Int8)",
            nullableParam = Database.ClickHouse.Params.int8,
            nullableResult = Database.ClickHouse.Result.nullable Database.ClickHouse.Result.int8,
            nullableExpected = Just (-1)
          },
        NullableTestCase
          { nullableType = "Nullable(Int16)",
            nullableParam = Database.ClickHouse.Params.int16,
            nullableResult = Database.ClickHouse.Result.nullable Database.ClickHouse.Result.int16,
            nullableExpected = Just (-100)
          },
        NullableTestCase
          { nullableType = "Nullable(Int32)",
            nullableParam = Database.ClickHouse.Params.int32,
            nullableResult = Database.ClickHouse.Result.nullable Database.ClickHouse.Result.int32,
            nullableExpected = Just (-100000)
          },
        NullableTestCase
          { nullableType = "Nullable(Int64)",
            nullableParam = Database.ClickHouse.Params.int64,
            nullableResult = Database.ClickHouse.Result.nullable Database.ClickHouse.Result.int64,
            nullableExpected = Just (-1000000)
          }
      ],
      -- Nullable Date
      [ NullableTestCase
          { nullableType = "Nullable(Date)",
            nullableParam = Database.ClickHouse.Params.day,
            nullableResult = Database.ClickHouse.Result.nullable Database.ClickHouse.Result.date,
            nullableExpected = Just (read "2024-01-31")
          },
        NullableTestCase
          { nullableType = "Nullable(Date32)",
            nullableParam = Database.ClickHouse.Params.day,
            nullableResult = Database.ClickHouse.Result.nullable Database.ClickHouse.Result.date32,
            nullableExpected = Just (read "2024-01-31")
          }
      ]
    ]

nullables :: IO Database.ClickHouse.Connection -> [Test.Tasty.TestTree]
nullables newConnection =
  -- Tests for Just values using parameterized queries
  [ Test.Tasty.testGroup
      (show nullableExpected <> "::" <> Data.Text.unpack nullableType)
      [ testCase "singleRow" $ do
          connection <- newConnection
          result <-
            Database.ClickHouse.runQuery
              connection
              ("SELECT { x : " <> nullableType <> "}")
              (nullableParam "x")
              (Database.ClickHouse.singleRow (Database.ClickHouse.Result.column nullableResult))
              expected
          nullableExpected @=? result
      ]
    | NullableTestCase {..} <- nullableTestCases,
      Just expected <- [nullableExpected]
  ]
    <>
    -- Tests for NULL values using literal SQL (can't pass NULL via params easily)
    [ Test.Tasty.testGroup
        ("NULL::" <> Data.Text.unpack type_)
        [ testCase "singleRow" $ do
            connection <- newConnection
            result <-
              Database.ClickHouse.runQuery
                connection
                ("SELECT NULL::" <> type_)
                mempty
                (Database.ClickHouse.singleRow (Database.ClickHouse.Result.column resultCol))
                ()
            Nothing @=? result
        ]
      | (type_, SomeColumn resultCol) <-
          [ ("Nullable(UInt8)", SomeColumn (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.uint8)),
            ("Nullable(UInt16)", SomeColumn (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.uint16)),
            ("Nullable(UInt32)", SomeColumn (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.uint32)),
            ("Nullable(UInt64)", SomeColumn (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.uint64)),
            ("Nullable(Int8)", SomeColumn (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.int8)),
            ("Nullable(Int16)", SomeColumn (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.int16)),
            ("Nullable(Int32)", SomeColumn (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.int32)),
            ("Nullable(Int64)", SomeColumn (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.int64)),
            ("Nullable(String)", SomeColumn (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.string)),
            ("Nullable(Date)", SomeColumn (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.date)),
            ("Nullable(Date32)", SomeColumn (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.date32))
          ]
    ]

data SomeColumn = forall a. (Eq a, Show a, Typeable a) => SomeColumn (Database.ClickHouse.Result.Column (Maybe a))

-- ---------------------------------------------------------------------------
-- Array tests
-- ---------------------------------------------------------------------------

data ArrayTestCase = forall a.
  (Eq a, Show a, Typeable a) =>
  ArrayTestCase
  { arrayQuery :: Text,
    arrayResult :: Database.ClickHouse.Result.Column a,
    arrayExpected :: a
  }

arrays :: IO Database.ClickHouse.Connection -> [Test.Tasty.TestTree]
arrays newConnection =
  [ Test.Tasty.testGroup
      label
      [ testCase "singleRow" $ do
          connection <- newConnection
          result <-
            Database.ClickHouse.runQuery
              connection
              arrayQuery
              mempty
              (Database.ClickHouse.singleRow (Database.ClickHouse.Result.column arrayResult))
              ()
          arrayExpected @=? result
      ]
    | (label, ArrayTestCase {..}) <- arrayTestCases
  ]

arrayTestCases :: [(String, ArrayTestCase)]
arrayTestCases =
  [ -- Empty arrays
    ( "empty Array(UInt32)",
      ArrayTestCase
        { arrayQuery = "SELECT []::Array(UInt32)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.uint32,
          arrayExpected = Data.Vector.empty :: Data.Vector.Vector Word32
        }
    ),
    ( "empty Array(String)",
      ArrayTestCase
        { arrayQuery = "SELECT []::Array(String)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.string,
          arrayExpected = Data.Vector.empty :: Data.Vector.Vector Text
        }
    ),
    -- Single element arrays
    ( "singleton Array(UInt8)",
      ArrayTestCase
        { arrayQuery = "SELECT [42]::Array(UInt8)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.uint8,
          arrayExpected = Data.Vector.fromList [42 :: Word8]
        }
    ),
    ( "singleton Array(String)",
      ArrayTestCase
        { arrayQuery = "SELECT ['hello']::Array(String)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.string,
          arrayExpected = Data.Vector.fromList ["hello" :: Text]
        }
    ),
    -- Multiple element arrays
    ( "Array(UInt8) [1,2,3]",
      ArrayTestCase
        { arrayQuery = "SELECT [1, 2, 3]::Array(UInt8)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.uint8,
          arrayExpected = Data.Vector.fromList [1, 2, 3 :: Word8]
        }
    ),
    ( "Array(UInt16) [0, 1000, 65535]",
      ArrayTestCase
        { arrayQuery = "SELECT [0, 1000, 65535]::Array(UInt16)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.uint16,
          arrayExpected = Data.Vector.fromList [0, 1000, 65535 :: Word16]
        }
    ),
    ( "Array(UInt32) [0, 100000, 4294967295]",
      ArrayTestCase
        { arrayQuery = "SELECT [0, 100000, 4294967295]::Array(UInt32)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.uint32,
          arrayExpected = Data.Vector.fromList [0, 100000, 4294967295 :: Word32]
        }
    ),
    ( "Array(UInt64) large values",
      ArrayTestCase
        { arrayQuery = "SELECT [0, 18446744073709551615]::Array(UInt64)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.uint64,
          arrayExpected = Data.Vector.fromList [0, 18446744073709551615 :: Word64]
        }
    ),
    ( "Array(Int8) [-128, 0, 127]",
      ArrayTestCase
        { arrayQuery = "SELECT [-128, 0, 127]::Array(Int8)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.int8,
          arrayExpected = Data.Vector.fromList [-128, 0, 127 :: Int8]
        }
    ),
    ( "Array(Int16) [-32768, 0, 32767]",
      ArrayTestCase
        { arrayQuery = "SELECT [-32768, 0, 32767]::Array(Int16)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.int16,
          arrayExpected = Data.Vector.fromList [-32768, 0, 32767 :: Int16]
        }
    ),
    ( "Array(Int32) [-2147483648, 0, 2147483647]",
      ArrayTestCase
        { arrayQuery = "SELECT [-2147483648, 0, 2147483647]::Array(Int32)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.int32,
          arrayExpected = Data.Vector.fromList [-2147483648, 0, 2147483647 :: Int32]
        }
    ),
    ( "Array(Int64) [-9223372036854775808, 0, 9223372036854775807]",
      ArrayTestCase
        { arrayQuery = "SELECT [-9223372036854775808, 0, 9223372036854775807]::Array(Int64)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.int64,
          arrayExpected = Data.Vector.fromList [-9223372036854775808, 0, 9223372036854775807 :: Int64]
        }
    ),
    ( "Array(String) multiple",
      ArrayTestCase
        { arrayQuery = "SELECT ['hello', 'world', 'clickhouse']::Array(String)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.string,
          arrayExpected = Data.Vector.fromList ["hello", "world", "clickhouse" :: Text]
        }
    ),
    ( "Array(String) unicode",
      ArrayTestCase
        { arrayQuery = "SELECT ['こんにちは', '😀😁']::Array(String)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.string,
          arrayExpected = Data.Vector.fromList ["こんにちは", "😀😁" :: Text]
        }
    ),
    ( "Array(Float32)",
      ArrayTestCase
        { arrayQuery = "SELECT [1.5, 2.5, 3.5]::Array(Float32)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.float32,
          arrayExpected = Data.Vector.fromList [1.5, 2.5, 3.5 :: Float]
        }
    ),
    ( "Array(Float64)",
      ArrayTestCase
        { arrayQuery = "SELECT [1.5, 2.5, 3.5]::Array(Float64)",
          arrayResult = Database.ClickHouse.Result.array Database.ClickHouse.Result.float64,
          arrayExpected = Data.Vector.fromList [1.5, 2.5, 3.5 :: Double]
        }
    ),
    -- Nested arrays
    ( "Array(Array(UInt32))",
      ArrayTestCase
        { arrayQuery = "SELECT [[1, 2], [3, 4, 5], []]::Array(Array(UInt32))",
          arrayResult = Database.ClickHouse.Result.array (Database.ClickHouse.Result.array Database.ClickHouse.Result.uint32),
          arrayExpected =
            Data.Vector.fromList
              [ Data.Vector.fromList [1, 2 :: Word32],
                Data.Vector.fromList [3, 4, 5],
                Data.Vector.empty
              ]
        }
    ),
    -- Array of Nullable
    ( "Array(Nullable(UInt32))",
      ArrayTestCase
        { arrayQuery = "SELECT [1, NULL, 3]::Array(Nullable(UInt32))",
          arrayResult = Database.ClickHouse.Result.array (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.uint32),
          arrayExpected = Data.Vector.fromList [Just 1, Nothing, Just (3 :: Word32)]
        }
    ),
    ( "Array(Nullable(String))",
      ArrayTestCase
        { arrayQuery = "SELECT ['hello', NULL, 'world']::Array(Nullable(String))",
          arrayResult = Database.ClickHouse.Result.array (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.string),
          arrayExpected = Data.Vector.fromList [Just "hello", Nothing, Just ("world" :: Text)]
        }
    )
  ]

-- ---------------------------------------------------------------------------
-- Map tests
-- ---------------------------------------------------------------------------

data MapTestCase = forall a.
  (Eq a, Show a, Typeable a) =>
  MapTestCase
  { mapQuery :: Text,
    mapResult :: Database.ClickHouse.Result.Column a,
    mapExpected :: a
  }

maps :: IO Database.ClickHouse.Connection -> [Test.Tasty.TestTree]
maps newConnection =
  [ Test.Tasty.testGroup
      label
      [ testCase "singleRow" $ do
          connection <- newConnection
          result <-
            Database.ClickHouse.runQuery
              connection
              mapQuery
              mempty
              (Database.ClickHouse.singleRow (Database.ClickHouse.Result.column mapResult))
              ()
          mapExpected @=? result
      ]
    | (label, MapTestCase {..}) <- mapTestCases
  ]

mapTestCases :: [(String, MapTestCase)]
mapTestCases =
  [ -- Empty map
    ( "empty Map(String, UInt32)",
      MapTestCase
        { mapQuery = "SELECT map()::Map(String, UInt32)",
          mapResult = Database.ClickHouse.Result.map Database.ClickHouse.Result.string Database.ClickHouse.Result.uint32,
          mapExpected = Data.HashMap.Strict.empty :: HashMap Text Word32
        }
    ),
    -- Single entry maps
    ( "singleton Map(String, UInt32)",
      MapTestCase
        { mapQuery = "SELECT map('key1', 42)::Map(String, UInt32)",
          mapResult = Database.ClickHouse.Result.map Database.ClickHouse.Result.string Database.ClickHouse.Result.uint32,
          mapExpected = Data.HashMap.Strict.fromList [("key1", 42 :: Word32)]
        }
    ),
    -- Multiple entries
    ( "Map(String, UInt32) multiple entries",
      MapTestCase
        { mapQuery = "SELECT map('a', 1, 'b', 2, 'c', 3)::Map(String, UInt32)",
          mapResult = Database.ClickHouse.Result.map Database.ClickHouse.Result.string Database.ClickHouse.Result.uint32,
          mapExpected = Data.HashMap.Strict.fromList [("a", 1), ("b", 2), ("c", 3 :: Word32)]
        }
    ),
    -- Map(String, String)
    ( "Map(String, String)",
      MapTestCase
        { mapQuery = "SELECT map('name', 'alice', 'city', 'tokyo')::Map(String, String)",
          mapResult = Database.ClickHouse.Result.map Database.ClickHouse.Result.string Database.ClickHouse.Result.string,
          mapExpected = Data.HashMap.Strict.fromList [("name", "alice"), ("city", "tokyo" :: Text)]
        }
    ),
    -- Map(String, Int64)
    ( "Map(String, Int64)",
      MapTestCase
        { mapQuery = "SELECT map('neg', -100, 'zero', 0, 'pos', 100)::Map(String, Int64)",
          mapResult = Database.ClickHouse.Result.map Database.ClickHouse.Result.string Database.ClickHouse.Result.int64,
          mapExpected = Data.HashMap.Strict.fromList [("neg", -100), ("zero", 0), ("pos", 100 :: Int64)]
        }
    ),
    -- Map(UInt32, String) - non-string keys
    ( "Map(UInt32, String)",
      MapTestCase
        { mapQuery = "SELECT map(1, 'one', 2, 'two', 3, 'three')::Map(UInt32, String)",
          mapResult = Database.ClickHouse.Result.map Database.ClickHouse.Result.uint32 Database.ClickHouse.Result.string,
          mapExpected = Data.HashMap.Strict.fromList [(1, "one"), (2, "two"), (3, "three" :: Text)] :: HashMap Word32 Text
        }
    ),
    -- Map(String, Float64)
    ( "Map(String, Float64)",
      MapTestCase
        { mapQuery = "SELECT map('pi', 3.14, 'e', 2.72)::Map(String, Float64)",
          mapResult = Database.ClickHouse.Result.map Database.ClickHouse.Result.string Database.ClickHouse.Result.float64,
          mapExpected = Data.HashMap.Strict.fromList [("pi", 3.14), ("e", 2.72 :: Double)]
        }
    ),
    -- Map with unicode keys
    ( "Map(String, UInt32) unicode keys",
      MapTestCase
        { mapQuery = "SELECT map('こんにちは', 1, '😀', 2)::Map(String, UInt32)",
          mapResult = Database.ClickHouse.Result.map Database.ClickHouse.Result.string Database.ClickHouse.Result.uint32,
          mapExpected = Data.HashMap.Strict.fromList [("こんにちは", 1), ("😀", 2 :: Word32)]
        }
    ),
    -- Map(String, Array(UInt32)) - map with array values
    ( "Map(String, Array(UInt32))",
      MapTestCase
        { mapQuery = "SELECT map('a', [1, 2, 3], 'b', [4, 5])::Map(String, Array(UInt32))",
          mapResult = Database.ClickHouse.Result.map Database.ClickHouse.Result.string (Database.ClickHouse.Result.array Database.ClickHouse.Result.uint32),
          mapExpected =
            Data.HashMap.Strict.fromList
              [ ("a", Data.Vector.fromList [1, 2, 3 :: Word32]),
                ("b", Data.Vector.fromList [4, 5])
              ]
        }
    ),
    -- Map(String, Nullable(UInt32)) - map with nullable values
    ( "Map(String, Nullable(UInt32))",
      MapTestCase
        { mapQuery = "SELECT map('present', 42, 'also', 99)::Map(String, Nullable(UInt32))",
          mapResult = Database.ClickHouse.Result.map Database.ClickHouse.Result.string (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.uint32),
          mapExpected = Data.HashMap.Strict.fromList [("present", Just 42), ("also", Just (99 :: Word32))]
        }
    )
  ]

-- ---------------------------------------------------------------------------
-- Value insert tests
-- ---------------------------------------------------------------------------

-- | Helper to create a table, insert a single row using Value, and read it back.
insertAndReadOne ::
  Database.ClickHouse.Connection ->
  Text ->
  Text ->
  Database.ClickHouse.Value.Value a ->
  Database.ClickHouse.Result.Column b ->
  a ->
  IO b
insertAndReadOne connection tableName columnDef valueEncoder resultDecoder row = do
  -- Create table
  Database.ClickHouse.runQuery
    connection
    ("CREATE TABLE IF NOT EXISTS " <> tableName <> " (val " <> columnDef <> ") ENGINE = Memory")
    mempty
    Database.ClickHouse.Result.noResult
    ()

  -- Insert single row
  let ins = Database.ClickHouse.Insert.insert tableName ["val"] valueEncoder mempty
  Database.ClickHouse.runInsert
    connection
    ins
    ()
    [row]

  -- Read back
  result <-
    Database.ClickHouse.runQuery
      connection
      ("SELECT val FROM " <> tableName)
      mempty
      (Database.ClickHouse.singleRow (Database.ClickHouse.Result.column resultDecoder))
      ()

  -- Drop table
  Database.ClickHouse.runQuery
    connection
    ("DROP TABLE " <> tableName)
    mempty
    Database.ClickHouse.Result.noResult
    ()

  pure result

valueInserts :: IO Database.ClickHouse.Connection -> [Test.Tasty.TestTree]
valueInserts newConnection =
  [ Test.Tasty.testGroup
      "Primitives"
      [ testCase "UInt8" $ do
          connection <- newConnection
          result <- insertAndReadOne connection "test_val_uint8" "UInt8" Database.ClickHouse.Value.uint8 Database.ClickHouse.Result.uint8 (42 :: Word8)
          (42 :: Word8) @=? result,
        testCase "UInt16" $ do
          connection <- newConnection
          result <- insertAndReadOne connection "test_val_uint16" "UInt16" Database.ClickHouse.Value.uint16 Database.ClickHouse.Result.uint16 (1000 :: Word16)
          (1000 :: Word16) @=? result,
        testCase "UInt32" $ do
          connection <- newConnection
          result <- insertAndReadOne connection "test_val_uint32" "UInt32" Database.ClickHouse.Value.uint32 Database.ClickHouse.Result.uint32 (100000 :: Word32)
          (100000 :: Word32) @=? result,
        testCase "UInt64" $ do
          connection <- newConnection
          result <- insertAndReadOne connection "test_val_uint64" "UInt64" Database.ClickHouse.Value.uint64 Database.ClickHouse.Result.uint64 (1000000 :: Word64)
          (1000000 :: Word64) @=? result,
        testCase "Int8" $ do
          connection <- newConnection
          result <- insertAndReadOne connection "test_val_int8" "Int8" Database.ClickHouse.Value.int8 Database.ClickHouse.Result.int8 (-42 :: Int8)
          (-42 :: Int8) @=? result,
        testCase "Int16" $ do
          connection <- newConnection
          result <- insertAndReadOne connection "test_val_int16" "Int16" Database.ClickHouse.Value.int16 Database.ClickHouse.Result.int16 (-1000 :: Int16)
          (-1000 :: Int16) @=? result,
        testCase "Int32" $ do
          connection <- newConnection
          result <- insertAndReadOne connection "test_val_int32" "Int32" Database.ClickHouse.Value.int32 Database.ClickHouse.Result.int32 (-100000 :: Int32)
          (-100000 :: Int32) @=? result,
        testCase "Int64" $ do
          connection <- newConnection
          result <- insertAndReadOne connection "test_val_int64" "Int64" Database.ClickHouse.Value.int64 Database.ClickHouse.Result.int64 (-1000000 :: Int64)
          (-1000000 :: Int64) @=? result,
        testCase "Float32" $ do
          connection <- newConnection
          result <- insertAndReadOne connection "test_val_float32" "Float32" Database.ClickHouse.Value.float32 Database.ClickHouse.Result.float32 (1.5 :: Float)
          (1.5 :: Float) @=? result,
        testCase "Float64" $ do
          connection <- newConnection
          result <- insertAndReadOne connection "test_val_float64" "Float64" Database.ClickHouse.Value.float64 Database.ClickHouse.Result.float64 (2.5 :: Double)
          (2.5 :: Double) @=? result,
        testCase "String" $ do
          connection <- newConnection
          result <- insertAndReadOne connection "test_val_string" "String" Database.ClickHouse.Value.string Database.ClickHouse.Result.string ("hello こんにちは" :: Text)
          ("hello こんにちは" :: Text) @=? result,
        testCase "Date" $ do
          connection <- newConnection
          let val = read "2024-01-31"
          result <- insertAndReadOne connection "test_val_date" "Date" Database.ClickHouse.Value.date Database.ClickHouse.Result.date val
          val @=? result,
        testCase "Date32" $ do
          connection <- newConnection
          let val = read "2024-01-31"
          result <- insertAndReadOne connection "test_val_date32" "Date32" Database.ClickHouse.Value.date32 Database.ClickHouse.Result.date32 val
          val @=? result,
        testCase "DateTime" $ do
          connection <- newConnection
          let val = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)
          result <- insertAndReadOne connection "test_val_datetime" "DateTime" Database.ClickHouse.Value.dateTime Database.ClickHouse.Result.dateTime val
          val @=? result,
        testCase "DateTime64(3)" $ do
          connection <- newConnection
          let val = UTCTime (fromGregorian 2024 6 15) (secondsToDiffTime 36000)
          result <- insertAndReadOne connection "test_val_datetime64" "DateTime64(3)" Database.ClickHouse.Value.dateTime64 Database.ClickHouse.Result.dateTime64 val
          val @=? result
      ],
    Test.Tasty.testGroup
      "Nullable"
      [ testCase "Nullable(UInt32) Just" $ do
          connection <- newConnection
          result <-
            insertAndReadOne
              connection
              "test_val_nullable_uint32_just"
              "Nullable(UInt32)"
              (Database.ClickHouse.Value.nullable Database.ClickHouse.Value.uint32)
              (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.uint32)
              (Just 42 :: Maybe Word32)
          Just (42 :: Word32) @=? result,
        testCase "Nullable(UInt32) Nothing" $ do
          connection <- newConnection
          result <-
            insertAndReadOne
              connection
              "test_val_nullable_uint32_nothing"
              "Nullable(UInt32)"
              (Database.ClickHouse.Value.nullable Database.ClickHouse.Value.uint32)
              (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.uint32)
              (Nothing :: Maybe Word32)
          (Nothing :: Maybe Word32) @=? result,
        testCase "Nullable(String) Just" $ do
          connection <- newConnection
          result <-
            insertAndReadOne
              connection
              "test_val_nullable_string_just"
              "Nullable(String)"
              (Database.ClickHouse.Value.nullable Database.ClickHouse.Value.string)
              (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.string)
              (Just "hello" :: Maybe Text)
          Just ("hello" :: Text) @=? result,
        testCase "Nullable(String) Nothing" $ do
          connection <- newConnection
          result <-
            insertAndReadOne
              connection
              "test_val_nullable_string_nothing"
              "Nullable(String)"
              (Database.ClickHouse.Value.nullable Database.ClickHouse.Value.string)
              (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.string)
              (Nothing :: Maybe Text)
          (Nothing :: Maybe Text) @=? result
      ],
    Test.Tasty.testGroup
      "Array"
      [ testCase "Array(UInt32)" $ do
          connection <- newConnection
          result <-
            insertAndReadOne
              connection
              "test_val_array_uint32"
              "Array(UInt32)"
              (Database.ClickHouse.Value.array Database.ClickHouse.Value.uint32)
              (Database.ClickHouse.Result.array Database.ClickHouse.Result.uint32)
              (Data.Vector.fromList [1, 2, 3 :: Word32])
          Data.Vector.fromList [1, 2, 3 :: Word32] @=? result,
        testCase "Array(UInt32) empty" $ do
          connection <- newConnection
          result <-
            insertAndReadOne
              connection
              "test_val_array_uint32_empty"
              "Array(UInt32)"
              (Database.ClickHouse.Value.array Database.ClickHouse.Value.uint32)
              (Database.ClickHouse.Result.array Database.ClickHouse.Result.uint32)
              (Data.Vector.empty :: Data.Vector.Vector Word32)
          (Data.Vector.empty :: Data.Vector.Vector Word32) @=? result,
        testCase "Array(String)" $ do
          connection <- newConnection
          result <-
            insertAndReadOne
              connection
              "test_val_array_string"
              "Array(String)"
              (Database.ClickHouse.Value.array Database.ClickHouse.Value.string)
              (Database.ClickHouse.Result.array Database.ClickHouse.Result.string)
              (Data.Vector.fromList ["hello", "world" :: Text])
          Data.Vector.fromList ["hello", "world" :: Text] @=? result,
        testCase "Array(Nullable(UInt32))" $ do
          connection <- newConnection
          result <-
            insertAndReadOne
              connection
              "test_val_array_nullable"
              "Array(Nullable(UInt32))"
              (Database.ClickHouse.Value.array (Database.ClickHouse.Value.nullable Database.ClickHouse.Value.uint32))
              (Database.ClickHouse.Result.array (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.uint32))
              (Data.Vector.fromList [Just 1, Nothing, Just (3 :: Word32)])
          Data.Vector.fromList [Just 1, Nothing, Just (3 :: Word32)] @=? result,
        testCase "Array(Array(UInt32)) nested" $ do
          connection <- newConnection
          result <-
            insertAndReadOne
              connection
              "test_val_array_nested"
              "Array(Array(UInt32))"
              (Database.ClickHouse.Value.array (Database.ClickHouse.Value.array Database.ClickHouse.Value.uint32))
              (Database.ClickHouse.Result.array (Database.ClickHouse.Result.array Database.ClickHouse.Result.uint32))
              ( Data.Vector.fromList
                  [ Data.Vector.fromList [1, 2 :: Word32],
                    Data.Vector.fromList [3, 4, 5],
                    Data.Vector.empty
                  ]
              )
          Data.Vector.fromList
            [ Data.Vector.fromList [1, 2 :: Word32],
              Data.Vector.fromList [3, 4, 5],
              Data.Vector.empty
            ]
            @=? result
      ],
    Test.Tasty.testGroup
      "Map"
      [ testCase "Map(String, UInt32)" $ do
          connection <- newConnection
          result <-
            insertAndReadOne
              connection
              "test_val_map_string_uint32"
              "Map(String, UInt32)"
              (Database.ClickHouse.Value.map Database.ClickHouse.Value.string Database.ClickHouse.Value.uint32)
              (Database.ClickHouse.Result.map Database.ClickHouse.Result.string Database.ClickHouse.Result.uint32)
              (Data.HashMap.Strict.fromList [("a", 1), ("b", 2), ("c", 3 :: Word32)])
          Data.HashMap.Strict.fromList [("a", 1), ("b", 2), ("c", 3 :: Word32)] @=? result,
        testCase "Map(String, String)" $ do
          connection <- newConnection
          result <-
            insertAndReadOne
              connection
              "test_val_map_string_string"
              "Map(String, String)"
              (Database.ClickHouse.Value.map Database.ClickHouse.Value.string Database.ClickHouse.Value.string)
              (Database.ClickHouse.Result.map Database.ClickHouse.Result.string Database.ClickHouse.Result.string)
              (Data.HashMap.Strict.fromList [("name", "alice"), ("city", "tokyo" :: Text)])
          Data.HashMap.Strict.fromList [("name", "alice"), ("city", "tokyo" :: Text)] @=? result,
        testCase "Map(String, Array(UInt32))" $ do
          connection <- newConnection
          result <-
            insertAndReadOne
              connection
              "test_val_map_array"
              "Map(String, Array(UInt32))"
              (Database.ClickHouse.Value.map Database.ClickHouse.Value.string (Database.ClickHouse.Value.array Database.ClickHouse.Value.uint32))
              (Database.ClickHouse.Result.map Database.ClickHouse.Result.string (Database.ClickHouse.Result.array Database.ClickHouse.Result.uint32))
              ( Data.HashMap.Strict.fromList
                  [ ("a", Data.Vector.fromList [1, 2, 3 :: Word32]),
                    ("b", Data.Vector.fromList [4, 5])
                  ]
              )
          Data.HashMap.Strict.fromList
            [ ("a", Data.Vector.fromList [1, 2, 3 :: Word32]),
              ("b", Data.Vector.fromList [4, 5])
            ]
            @=? result,
        testCase "empty Map(String, UInt32)" $ do
          connection <- newConnection
          result <-
            insertAndReadOne
              connection
              "test_val_map_empty"
              "Map(String, UInt32)"
              (Database.ClickHouse.Value.map Database.ClickHouse.Value.string Database.ClickHouse.Value.uint32)
              (Database.ClickHouse.Result.map Database.ClickHouse.Result.string Database.ClickHouse.Result.uint32)
              (Data.HashMap.Strict.empty :: HashMap Text Word32)
          (Data.HashMap.Strict.empty :: HashMap Text Word32) @=? result
      ]
  ]

-- ---------------------------------------------------------------------------
-- Stress test roundtrip (100k rows)
-- ---------------------------------------------------------------------------

-- | Helper to create a table, insert many rows using Value, and read them all back.
insertAndReadMany ::
  (Eq b, Show b) =>
  Database.ClickHouse.Connection ->
  Text ->
  Text ->
  Database.ClickHouse.Value.Value a ->
  Database.ClickHouse.Result.Column b ->
  [a] ->
  (a -> b) ->
  IO (Data.Vector.Vector b)
insertAndReadMany connection tableName columnDef valueEncoder resultDecoder rows _toResult = do
  -- Create table
  Database.ClickHouse.runQuery
    connection
    ("CREATE TABLE IF NOT EXISTS " <> tableName <> " (val " <> columnDef <> ") ENGINE = Memory")
    mempty
    Database.ClickHouse.Result.noResult
    ()

  -- Insert all rows
  let ins = Database.ClickHouse.Insert.insert tableName ["val"] valueEncoder mempty
  Database.ClickHouse.runInsert
    connection
    ins
    ()
    rows

  -- Read back all rows
  result <-
    Database.ClickHouse.runQuery
      connection
      ("SELECT val FROM " <> tableName)
      mempty
      (Database.ClickHouse.manyRows (Database.ClickHouse.Result.column resultDecoder))
      ()

  -- Drop table
  Database.ClickHouse.runQuery
    connection
    ("DROP TABLE " <> tableName)
    mempty
    Database.ClickHouse.Result.noResult
    ()

  pure result

stressTests :: IO Database.ClickHouse.Connection -> [Test.Tasty.TestTree]
stressTests newConnection =
  [ testCase "100k Int64 roundtrip" $ do
      connection <- newConnection
      let n = 100000 :: Int
          rows = [0 .. fromIntegral (n - 1) :: Int64]
      result <-
        insertAndReadMany
          connection
          "stress_int64"
          "Int64"
          Database.ClickHouse.Value.int64
          Database.ClickHouse.Result.int64
          rows
          id
      Data.Vector.fromList rows @=? result,
    testCase "100k UInt64 roundtrip" $ do
      connection <- newConnection
      let n = 100000 :: Int
          rows = [0 .. fromIntegral (n - 1) :: Word64]
      result <-
        insertAndReadMany
          connection
          "stress_uint64"
          "UInt64"
          Database.ClickHouse.Value.uint64
          Database.ClickHouse.Result.uint64
          rows
          id
      Data.Vector.fromList rows @=? result,
    testCase "100k String roundtrip" $ do
      connection <- newConnection
      let n = 100000 :: Int
          rows = [Data.Text.pack ("row-" <> show i) | i <- [0 .. n - 1 :: Int]]
      result <-
        insertAndReadMany
          connection
          "stress_string"
          "String"
          Database.ClickHouse.Value.string
          Database.ClickHouse.Result.string
          rows
          id
      Data.Vector.fromList rows @=? result,
    testCase "100k Nullable(Int32) roundtrip" $ do
      connection <- newConnection
      let n = 100000 :: Int
          rows =
            [ if mod i 10 == 0 then Nothing else Just (fromIntegral i :: Int32)
              | i <- [0 .. n - 1 :: Int]
            ]
      result <-
        insertAndReadMany
          connection
          "stress_nullable_int32"
          "Nullable(Int32)"
          (Database.ClickHouse.Value.nullable Database.ClickHouse.Value.int32)
          (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.int32)
          rows
          id
      Data.Vector.fromList rows @=? result,
    testCase "100k Float64 roundtrip" $ do
      connection <- newConnection
      let n = 100000 :: Int
          rows = [fromIntegral i * 0.5 :: Double | i <- [0 .. n - 1 :: Int]]
      result <-
        insertAndReadMany
          connection
          "stress_float64"
          "Float64"
          Database.ClickHouse.Value.float64
          Database.ClickHouse.Result.float64
          rows
          id
      Data.Vector.fromList rows @=? result,
    testCase "100k multi-column (Int64, String, Nullable(UInt32)) roundtrip" $ do
      connection <- newConnection
      let n = 100000 :: Int

      -- Create table with multiple columns
      Database.ClickHouse.runQuery
        connection
        "CREATE TABLE IF NOT EXISTS stress_multi (a Int64, b String, c Nullable(UInt32)) ENGINE = Memory"
        mempty
        Database.ClickHouse.Result.noResult
        ()

      -- Build rows as tuples
      let rows =
            [ ( fromIntegral i :: Int64,
                Data.Text.pack ("s-" <> show i),
                if mod i 7 == 0 then Nothing else Just (fromIntegral i :: Word32)
              )
              | i <- [0 .. n - 1 :: Int]
            ]

      let valueEncoder =
            contramap (\(a, _, _) -> a) Database.ClickHouse.Value.int64
              <> contramap (\(_, b, _) -> b) Database.ClickHouse.Value.string
              <> contramap (\(_, _, c) -> c) (Database.ClickHouse.Value.nullable Database.ClickHouse.Value.uint32)

      let ins = Database.ClickHouse.Insert.insert "stress_multi" ["a", "b", "c"] valueEncoder mempty
      Database.ClickHouse.runInsert
        connection
        ins
        ()
        rows

      -- Read back
      result <-
        Database.ClickHouse.runQuery
          connection
          "SELECT a, b, c FROM stress_multi"
          mempty
          ( Database.ClickHouse.manyRows
              ( (,,)
                  <$> Database.ClickHouse.Result.column Database.ClickHouse.Result.int64
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.uint32)
              )
          )
          ()

      -- Drop table
      Database.ClickHouse.runQuery
        connection
        "DROP TABLE stress_multi"
        mempty
        Database.ClickHouse.Result.noResult
        ()

      Data.Vector.fromList rows @=? result
  ]

-- ---------------------------------------------------------------------------
-- Wide table smoketest
-- ---------------------------------------------------------------------------

-- | Smoketest that creates a wide table with many column types (including
-- LowCardinality, Nullable, Array, Map, Enum8, Bool, UUID, DateTime with
-- timezone, etc.) to verify the library handles a realistic schema.
wideTableSmoketest :: IO Database.ClickHouse.Connection -> [Test.Tasty.TestTree]
wideTableSmoketest newConnection =
  [ testCase "insert and read wide table" $ do
      connection <- newConnection

      -- Create a wide table exercising many ClickHouse types
      Database.ClickHouse.runQuery
        connection
        ( Data.Text.unlines
            [ "CREATE TABLE IF NOT EXISTS wide_smoketest (",
              "  col_revision UInt8,",
              "  col_time DateTime('UTC'),",
              "  col_entity_id UInt64,",
              "  col_name String DEFAULT '',",
              "  col_lc_uuid UUID DEFAULT '00000000-0000-0000-0000-000000000000',",
              "  col_label String DEFAULT '',",
              "  col_lc_uuid_2 UUID DEFAULT '00000000-0000-0000-0000-000000000000',",
              "  col_description String DEFAULT '',",
              "  col_url String DEFAULT '',",
              "  col_domain String DEFAULT '',",
              "  col_image String DEFAULT '',",
              "  col_reference String DEFAULT '',",
              "  col_backend String DEFAULT '',",
              "  col_version String DEFAULT '',",
              "  col_file_name String DEFAULT '',",
              "  col_redirect_url String DEFAULT '',",
              "  col_lib_version String DEFAULT '',",
              "  col_arch String DEFAULT '',",
              "  col_platform_name String DEFAULT '',",
              "  col_variables Map(String, String) DEFAULT map(),",
              "  col_imported_version String DEFAULT '',",
              "  col_dnt Bool DEFAULT false,",
              "  col_is_bot Nullable(Bool) DEFAULT false,",
              "  col_os_name String DEFAULT '',",
              "  col_client_name String DEFAULT '',",
              "  col_client_version String DEFAULT '',",
              "  col_request_id String DEFAULT '',",
              "  col_request_method LowCardinality(String) DEFAULT '',",
              "  col_request_host String DEFAULT '',",
              "  col_request_path String DEFAULT '',",
              "  col_request_query String DEFAULT '',",
              "  col_remote_address String DEFAULT '',",
              "  col_response_status UInt16 DEFAULT 0,",
              "  col_origin_id UUID DEFAULT '00000000-0000-0000-0000-000000000000',",
              "  col_endpoint_id String DEFAULT '',",
              "  col_endpoint_source String DEFAULT '',",
              "  col_endpoint_type LowCardinality(String) DEFAULT '',",
              "  col_endpoint_domain String DEFAULT '',",
              "  col_endpoint_name String DEFAULT '',",
              "  col_geo_country String DEFAULT '',",
              "  col_geo_state String DEFAULT '',",
              "  col_geo_city String DEFAULT '',",
              "  col_geo_postal_code String DEFAULT '',",
              "  col_geo_latitude Float64 DEFAULT 0.,",
              "  col_geo_longitude Float64 DEFAULT 0.,",
              "  col_company_id UInt64 DEFAULT 0,",
              "  col_priority LowCardinality(String) DEFAULT '',",
              "  col_score UInt64 DEFAULT 0,",
              "  col_os String DEFAULT '',",
              "  col_user_agent String DEFAULT '',",
              "  col_confidence Float64 DEFAULT 0.,",
              "  col_event_type Enum8('unknown' = 1, 'type_a' = 2, 'type_b' = 3, 'type_c' = 4, 'type_d' = 5, 'type_e' = 6, 'type_f' = 7, 'type_g' = 8) DEFAULT 1,",
              "  col_imported_type String DEFAULT '',",
              "  col_inserted_at DateTime('UTC') DEFAULT now(),",
              "  col_tag LowCardinality(String) DEFAULT 'neutral',",
              "  col_tag_user Int32 DEFAULT 0,",
              "  col_provenance LowCardinality(String) DEFAULT 'normal',",
              "  col_tag_id Int64 DEFAULT 0,",
              "  col_tag_updated_at DateTime('UTC') DEFAULT '1970-01-01 00:00:00',",
              "  col_is_vpn Bool DEFAULT false,",
              "  col_is_cloud Bool DEFAULT false,",
              "  col_global_tag LowCardinality(String) DEFAULT 'neutral',",
              "  col_global_tag_user Int64 DEFAULT 0,",
              "  col_global_tag_id Int64 DEFAULT 0,",
              "  col_global_tag_updated_at DateTime('UTC') DEFAULT '1970-01-01 00:00:00',",
              "  col_count Int64 DEFAULT 1,",
              "  col_tuple2 Tuple(Int32, String),",
              "  col_tuple3 Tuple(Float64, UInt64, String),",
              "  col_tuple2b Tuple(Bool, Int64),",
              "  col_array_tuple Array(Tuple(String, Int32))",
              ") ENGINE = Memory"
            ]
        )
        mempty
        Database.ClickHouse.Result.noResult
        ()

      -- Build the value encoder for all columns
      let valueEncoder =
            contramap (\r -> col_revision r) Database.ClickHouse.Value.uint8
              <> contramap (\r -> col_time r) Database.ClickHouse.Value.dateTime
              <> contramap (\r -> col_entity_id r) Database.ClickHouse.Value.uint64
              <> contramap (\r -> col_name r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_lc_uuid r) Database.ClickHouse.Value.uuid
              <> contramap (\r -> col_label r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_lc_uuid_2 r) Database.ClickHouse.Value.uuid
              <> contramap (\r -> col_description r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_url r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_domain r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_image r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_reference r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_backend r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_version r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_file_name r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_redirect_url r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_lib_version r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_arch r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_platform_name r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_variables r) (Database.ClickHouse.Value.map Database.ClickHouse.Value.string Database.ClickHouse.Value.string)
              <> contramap (\r -> col_imported_version r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_dnt r) Database.ClickHouse.Value.bool
              <> contramap (\r -> col_is_bot r) (Database.ClickHouse.Value.nullable Database.ClickHouse.Value.bool)
              <> contramap (\r -> col_os_name r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_client_name r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_client_version r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_request_id r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_request_method r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_request_host r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_request_path r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_request_query r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_remote_address r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_response_status r) Database.ClickHouse.Value.uint16
              <> contramap (\r -> col_origin_id r) Database.ClickHouse.Value.uuid
              <> contramap (\r -> col_endpoint_id r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_endpoint_source r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_endpoint_type r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_endpoint_domain r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_endpoint_name r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_geo_country r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_geo_state r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_geo_city r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_geo_postal_code r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_geo_latitude r) Database.ClickHouse.Value.float64
              <> contramap (\r -> col_geo_longitude r) Database.ClickHouse.Value.float64
              <> contramap (\r -> col_company_id r) Database.ClickHouse.Value.uint64
              <> contramap (\r -> col_priority r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_score r) Database.ClickHouse.Value.uint64
              <> contramap (\r -> col_os r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_user_agent r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_confidence r) Database.ClickHouse.Value.float64
              <> contramap (\r -> col_event_type r) Database.ClickHouse.Value.int8
              <> contramap (\r -> col_imported_type r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_inserted_at r) Database.ClickHouse.Value.dateTime
              <> contramap (\r -> col_tag r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_tag_user r) Database.ClickHouse.Value.int32
              <> contramap (\r -> col_provenance r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_tag_id r) Database.ClickHouse.Value.int64
              <> contramap (\r -> col_tag_updated_at r) Database.ClickHouse.Value.dateTime
              <> contramap (\r -> col_is_vpn r) Database.ClickHouse.Value.bool
              <> contramap (\r -> col_is_cloud r) Database.ClickHouse.Value.bool
              <> contramap (\r -> col_global_tag r) Database.ClickHouse.Value.string
              <> contramap (\r -> col_global_tag_user r) Database.ClickHouse.Value.int64
              <> contramap (\r -> col_global_tag_id r) Database.ClickHouse.Value.int64
              <> contramap (\r -> col_global_tag_updated_at r) Database.ClickHouse.Value.dateTime
              <> contramap (\r -> col_count r) Database.ClickHouse.Value.int64
              <> contramap (\r -> col_tuple2 r) (Database.ClickHouse.Value.tuple Database.ClickHouse.Value.int32 Database.ClickHouse.Value.string)
              <> contramap (\r -> col_tuple3 r) (Database.ClickHouse.Value.tuple3 Database.ClickHouse.Value.float64 Database.ClickHouse.Value.uint64 Database.ClickHouse.Value.string)
              <> contramap (\r -> col_tuple2b r) (Database.ClickHouse.Value.tuple Database.ClickHouse.Value.bool Database.ClickHouse.Value.int64)
              <> contramap (\r -> col_array_tuple r) (Database.ClickHouse.Value.array (Database.ClickHouse.Value.tuple Database.ClickHouse.Value.string Database.ClickHouse.Value.int32))

      let columnNames =
            [ "col_revision",
              "col_time",
              "col_entity_id",
              "col_name",
              "col_lc_uuid",
              "col_label",
              "col_lc_uuid_2",
              "col_description",
              "col_url",
              "col_domain",
              "col_image",
              "col_reference",
              "col_backend",
              "col_version",
              "col_file_name",
              "col_redirect_url",
              "col_lib_version",
              "col_arch",
              "col_platform_name",
              "col_variables",
              "col_imported_version",
              "col_dnt",
              "col_is_bot",
              "col_os_name",
              "col_client_name",
              "col_client_version",
              "col_request_id",
              "col_request_method",
              "col_request_host",
              "col_request_path",
              "col_request_query",
              "col_remote_address",
              "col_response_status",
              "col_origin_id",
              "col_endpoint_id",
              "col_endpoint_source",
              "col_endpoint_type",
              "col_endpoint_domain",
              "col_endpoint_name",
              "col_geo_country",
              "col_geo_state",
              "col_geo_city",
              "col_geo_postal_code",
              "col_geo_latitude",
              "col_geo_longitude",
              "col_company_id",
              "col_priority",
              "col_score",
              "col_os",
              "col_user_agent",
              "col_confidence",
              "col_event_type",
              "col_imported_type",
              "col_inserted_at",
              "col_tag",
              "col_tag_user",
              "col_provenance",
              "col_tag_id",
              "col_tag_updated_at",
              "col_is_vpn",
              "col_is_cloud",
              "col_global_tag",
              "col_global_tag_user",
              "col_global_tag_id",
              "col_global_tag_updated_at",
              "col_count",
              "col_tuple2",
              "col_tuple3",
              "col_tuple2b",
              "col_array_tuple"
            ]

      let ins = Database.ClickHouse.Insert.insert "wide_smoketest" columnNames valueEncoder mempty

      let testUUID = case Data.UUID.fromString "a1b2c3d4-e5f6-7890-abcd-ef1234567890" of
            Just u -> u
            Nothing -> error "invalid UUID"

      let epoch = UTCTime (fromGregorian 1970 1 1) 0
      let testTime = UTCTime (fromGregorian 2024 3 15) (secondsToDiffTime 43200)
      let longStr = Data.Text.replicate 200 "abcdefghij" -- 2000 chars
      let row =
            WideRow
              { col_revision = 10,
                col_time = testTime,
                col_entity_id = 42,
                col_name = longStr,
                col_lc_uuid = testUUID,
                col_label = longStr,
                col_lc_uuid_2 = testUUID,
                col_description = longStr,
                col_url = longStr,
                col_domain = longStr,
                col_image = longStr,
                col_reference = longStr,
                col_backend = longStr,
                col_version = longStr,
                col_file_name = longStr,
                col_redirect_url = longStr,
                col_lib_version = longStr,
                col_arch = longStr,
                col_platform_name = longStr,
                col_variables = Data.HashMap.Strict.fromList [("foo", longStr), ("version", longStr)],
                col_imported_version = longStr,
                col_dnt = False,
                col_is_bot = Just False,
                col_os_name = longStr,
                col_client_name = longStr,
                col_client_version = longStr,
                col_request_id = longStr,
                col_request_method = "GET",
                col_request_host = longStr,
                col_request_path = longStr,
                col_request_query = longStr,
                col_remote_address = longStr,
                col_response_status = 200,
                col_origin_id = testUUID,
                col_endpoint_id = longStr,
                col_endpoint_source = longStr,
                col_endpoint_type = longStr,
                col_endpoint_domain = longStr,
                col_endpoint_name = longStr,
                col_geo_country = longStr,
                col_geo_state = longStr,
                col_geo_city = longStr,
                col_geo_postal_code = longStr,
                col_geo_latitude = 37.77,
                col_geo_longitude = -122.39,
                col_company_id = 1234,
                col_priority = longStr,
                col_score = 500,
                col_os = longStr,
                col_user_agent = longStr,
                col_confidence = 0.85,
                col_event_type = 2,
                col_imported_type = longStr,
                col_inserted_at = testTime,
                col_tag = longStr,
                col_tag_user = 0,
                col_provenance = longStr,
                col_tag_id = 0,
                col_tag_updated_at = epoch,
                col_is_vpn = False,
                col_is_cloud = False,
                col_global_tag = longStr,
                col_global_tag_user = 0,
                col_global_tag_id = 0,
                col_global_tag_updated_at = epoch,
                col_count = 1,
                col_tuple2 = (42, "hello"),
                col_tuple3 = (3.14, 999, "world"),
                col_tuple2b = (True, -100),
                col_array_tuple = Data.Vector.fromList [("alpha", 1), ("beta", 2), ("gamma", 3)]
              }

      let n = 10000 :: Int
      Database.ClickHouse.runInsert connection ins () (replicate n row)

      -- Read back using a multi-column Row decoder
      result <-
        Database.ClickHouse.runQuery
          connection
          ("SELECT " <> Data.Text.intercalate ", " columnNames <> " FROM wide_smoketest")
          mempty
          ( Database.ClickHouse.manyRows
              ( WideRow
                  <$> Database.ClickHouse.Result.column Database.ClickHouse.Result.uint8
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.dateTime
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.uint64
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.uuid
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.uuid
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column (Database.ClickHouse.Result.map Database.ClickHouse.Result.string Database.ClickHouse.Result.string)
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.bool
                  <*> Database.ClickHouse.Result.column (Database.ClickHouse.Result.nullable Database.ClickHouse.Result.bool)
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.uint16
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.uuid
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.float64
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.float64
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.uint64
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.uint64
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.float64
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.int8
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.dateTime
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.int32
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.int64
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.dateTime
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.bool
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.bool
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.string
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.int64
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.int64
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.dateTime
                  <*> Database.ClickHouse.Result.column Database.ClickHouse.Result.int64
                  <*> Database.ClickHouse.Result.column ((,) <$> Database.ClickHouse.Result.int32 <*> Database.ClickHouse.Result.string)
                  <*> Database.ClickHouse.Result.column ((,,) <$> Database.ClickHouse.Result.float64 <*> Database.ClickHouse.Result.uint64 <*> Database.ClickHouse.Result.string)
                  <*> Database.ClickHouse.Result.column ((,) <$> Database.ClickHouse.Result.bool <*> Database.ClickHouse.Result.int64)
                  <*> Database.ClickHouse.Result.column (Database.ClickHouse.Result.array ((,) <$> Database.ClickHouse.Result.string <*> Database.ClickHouse.Result.int32))
              )
          )
          ()

      Data.Vector.fromList (replicate n row) @=? result

      -- Drop table
      Database.ClickHouse.runQuery
        connection
        "DROP TABLE wide_smoketest"
        mempty
        Database.ClickHouse.Result.noResult
        ()
  ]

data WideRow = WideRow
  { col_revision :: Word8,
    col_time :: UTCTime,
    col_entity_id :: Word64,
    col_name :: Text,
    col_lc_uuid :: UUID,
    col_label :: Text,
    col_lc_uuid_2 :: UUID,
    col_description :: Text,
    col_url :: Text,
    col_domain :: Text,
    col_image :: Text,
    col_reference :: Text,
    col_backend :: Text,
    col_version :: Text,
    col_file_name :: Text,
    col_redirect_url :: Text,
    col_lib_version :: Text,
    col_arch :: Text,
    col_platform_name :: Text,
    col_variables :: HashMap Text Text,
    col_imported_version :: Text,
    col_dnt :: Bool,
    col_is_bot :: Maybe Bool,
    col_os_name :: Text,
    col_client_name :: Text,
    col_client_version :: Text,
    col_request_id :: Text,
    col_request_method :: Text,
    col_request_host :: Text,
    col_request_path :: Text,
    col_request_query :: Text,
    col_remote_address :: Text,
    col_response_status :: Word16,
    col_origin_id :: UUID,
    col_endpoint_id :: Text,
    col_endpoint_source :: Text,
    col_endpoint_type :: Text,
    col_endpoint_domain :: Text,
    col_endpoint_name :: Text,
    col_geo_country :: Text,
    col_geo_state :: Text,
    col_geo_city :: Text,
    col_geo_postal_code :: Text,
    col_geo_latitude :: Double,
    col_geo_longitude :: Double,
    col_company_id :: Word64,
    col_priority :: Text,
    col_score :: Word64,
    col_os :: Text,
    col_user_agent :: Text,
    col_confidence :: Double,
    col_event_type :: Int8,
    col_imported_type :: Text,
    col_inserted_at :: UTCTime,
    col_tag :: Text,
    col_tag_user :: Int32,
    col_provenance :: Text,
    col_tag_id :: Int64,
    col_tag_updated_at :: UTCTime,
    col_is_vpn :: Bool,
    col_is_cloud :: Bool,
    col_global_tag :: Text,
    col_global_tag_user :: Int64,
    col_global_tag_id :: Int64,
    col_global_tag_updated_at :: UTCTime,
    col_count :: Int64,
    col_tuple2 :: (Int32, Text),
    col_tuple3 :: (Double, Word64, Text),
    col_tuple2b :: (Bool, Int64),
    col_array_tuple :: Data.Vector.Vector (Text, Int32)
  }
  deriving (Eq, Show)
