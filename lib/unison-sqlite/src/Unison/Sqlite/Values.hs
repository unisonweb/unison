module Unison.Sqlite.Values
  ( Values (..),
    valuesSql,
  )
where

import Data.List.NonEmpty qualified as List (NonEmpty)
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Text qualified as Text
import Database.SQLite.Simple qualified as Sqlite.Simple
import Unison.Prelude
import Unison.Sqlite.Sql (Sql (..))

-- | A @VALUES@ literal.
newtype Values a
  = Values (List.NonEmpty a)
  deriving stock (Show)

instance (Sqlite.Simple.ToRow a) => Sqlite.Simple.ToRow (Values a) where
  toRow (Values values) =
    foldMap Sqlite.Simple.toRow values

-- | Example: given a 'Values' of length 3, where each element has a @toRow@ that produces 2 elements, produce the SQL
-- string:
--
-- @
-- VALUES (?, ?), (?, ?), (?, ?)
-- @
valuesSql :: (Sqlite.Simple.ToRow a) => Values a -> Sql
valuesSql (Values values) =
  Sql ("VALUES " <> Text.intercalate "," (replicate (length values) (valueSql columns)))
  where
    columns :: Int
    columns =
      length (Sqlite.Simple.toRow (List.NonEmpty.head values))

valueSql :: Int -> Text
valueSql columns =
  "(" <> Text.intercalate "," (replicate columns "?") <> ")"
