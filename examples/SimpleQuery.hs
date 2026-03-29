{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Database.ClickHouse
import Database.ClickHouse.Params qualified as Params
import Database.ClickHouse.Result qualified as Result

main :: IO ()
main = do
  connection <-
    newConnection
      ConnectionOptions
        { url = "http://localhost:8123",
          database = Nothing,
          user = Nothing,
          password = Nothing,
          httpManager = Nothing
        }

  answer <-
    runQuery
      connection
      "SELECT { x : UInt64 }"
      (Params.uint64 "x")
      (singleRow (Result.column Result.uint64))
      42

  print answer
