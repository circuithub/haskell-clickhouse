module Main (main) where

import Data.Foldable (toList)
import Data.Functor.Contravariant (($<))
import Data.Text (Text)
import Data.Text qualified
import Data.Typeable (Typeable)
import Data.Typeable qualified
import Database.ClickHouse qualified
import Database.ClickHouse.Params qualified
import Database.ClickHouse.Result qualified
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
    [ Test.Tasty.testGroup "Primitive data types" (primitives newConnection)
    ]

data PrimitiveTestCase
  = forall a.
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
    mkD32 (read "2024-01-31")
    -- TODO DateTime/DateTime32/DateTime64
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
