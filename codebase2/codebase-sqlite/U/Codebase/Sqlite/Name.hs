{-# LANGUAGE RecordWildCards #-}

module U.Codebase.Sqlite.Name where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import Unison.Name (Name)
import qualified Unison.Name as Name
import Unison.NameSegment (NameSegment)
import qualified Unison.NameSegment as NameSegment
import Unison.Prelude
import Unison.Sqlite (FromRow (..), ToField (..), ToRow (..), field)

newtype NamedRef ref = NamedRef (Name, ref)
  deriving stock (Show, Functor, Foldable, Traversable)

instance ToRow ref => ToRow (NamedRef ref) where
  toRow (NamedRef (name, ref)) =
    [toField (Text.intercalate "." . nameSegmentsToText . toList . Name.reverseSegments $ name)] <> toRow ref
    where
      nameSegmentsToText :: [NameSegment] -> [Text]
      nameSegmentsToText = coerce

instance FromRow ref => FromRow (NamedRef ref) where
  fromRow = do
    reversedNameSegments <- NonEmpty.fromList . Text.splitOn "." <$> field
    ref <- fromRow
    pure (NamedRef (Name.fromReverseSegments (coerce @(NonEmpty Text) @(NonEmpty NameSegment) reversedNameSegments), ref))
