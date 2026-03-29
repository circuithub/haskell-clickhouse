{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.ClickHouse.Insert
  ( Insert (..),
    insert,
    modifySettings,
    renderInsert,
  )
where

import Data.Foldable1 (intercalate1)
import Data.List (intersperse)
import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import Data.Text.Lazy.Builder qualified
import Database.ClickHouse.Params (Param)
import Database.ClickHouse.Value (Value)

data Insert input a = Insert
  { tableName :: Text,
    columnNames :: [Text],
    encoder :: Value a,
    params :: Param input,
    settings :: [(Text, Text)]
  }

insert :: Text -> [Text] -> Value a -> Param input -> Insert input a
insert tableName columnNames encoder params =
  Insert {tableName, columnNames, encoder, params, settings = mempty}

modifySettings :: ([(Text, Text)] -> [(Text, Text)]) -> Insert input a -> Insert input a
modifySettings modify insert =
  insert {settings = modify (settings insert)}

renderInsert :: Insert input a -> Data.Text.Lazy.Builder.Builder
renderInsert Insert {..} =
  mconcat $
    intersperse
      " "
      [ "INSERT INTO",
        renderedTableName,
        renderedColumnNames,
        renderedSettings,
        "FORMAT RowBinary"
      ]
  where
    renderedTableName =
      Data.Text.Lazy.Builder.fromText tableName

    renderedColumnNames =
      case nonEmpty columnNames of
        Nothing ->
          mempty
        Just columnNames ->
          "(\"" <> intercalate1 "\", \"" (fmap Data.Text.Lazy.Builder.fromText columnNames) <> "\")"

    renderedSettings =
      case nonEmpty settings of
        Nothing ->
          mempty
        Just settings ->
          "SETTINGS "
            <> intercalate1
              ", "
              (fmap (\(k, v) -> Data.Text.Lazy.Builder.fromText k <> " = " <> Data.Text.Lazy.Builder.fromText v) settings)
