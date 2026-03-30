{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Functor.Contravariant (contramap)
import Database.ClickHouse
import Database.ClickHouse.Value qualified as Value

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

  let rowEncoder =
        contramap fst Value.uint32
          <> contramap snd Value.string

      ins = insert "events" ["id", "name"] rowEncoder mempty

  runInsert connection ins () [(1, "signup"), (2, "purchase")]
