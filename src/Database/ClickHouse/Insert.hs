{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.ClickHouse.Insert where

import Data.Foldable1 (intercalate1)
import Data.List (intersperse)
import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import Data.Text.Lazy.Builder qualified
import Database.ClickHouse.Value (Value)

data Insert a = Insert
  { tableName :: Text,
    columnNames :: [Text],
    encoder :: Value a,
    settings :: [(Text, Text)]
  }

insert :: Text -> [Text] -> Value a -> Insert a
insert tableName columnNames encoder =
  Insert {tableName, columnNames, encoder, settings = mempty}

modifySettings :: ([(Text, Text)] -> [(Text, Text)]) -> Insert a -> Insert a
modifySettings modify insert =
  insert {settings = modify (settings insert)}

renderInsert :: Insert a -> Data.Text.Lazy.Builder.Builder
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
        Just _settings ->
          "SETTINGS"
