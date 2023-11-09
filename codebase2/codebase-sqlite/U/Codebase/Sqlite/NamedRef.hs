{-# OPTIONS_GHC -Wno-orphans #-}

module U.Codebase.Sqlite.NamedRef where

import Data.List.NonEmpty qualified as NEL
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import U.Codebase.Sqlite.NameLookups (ReversedName)
import Unison.ConstructorType (ConstructorType)
import Unison.ConstructorType qualified as ConstructorType
import Unison.Prelude
import Unison.Sqlite

instance ToField ConstructorType where
  toField = \case
    ConstructorType.Data -> SQLInteger 0
    ConstructorType.Effect -> SQLInteger 1

instance FromField ConstructorType where
  fromField f =
    fromField @Int f >>= \case
      0 -> pure ConstructorType.Data
      1 -> pure ConstructorType.Effect
      _ -> fail "Invalid ConstructorType"

data NamedRef ref = NamedRef {reversedSegments :: ReversedName, ref :: ref}
  deriving stock (Show, Functor, Foldable, Traversable)

instance (ToRow ref) => ToRow (NamedRef ref) where
  toRow (NamedRef {reversedSegments = segments, ref}) =
    [toField reversedName] <> toRow ref
    where
      reversedName =
        segments
          & into @[Text]
          & Text.intercalate "."
          & (<> ".") -- Add trailing dot, see notes on scoped_term_name_lookup schema

instance (FromRow ref) => FromRow (NamedRef ref) where
  fromRow = do
    reversedSegments <-
      field <&> \f ->
        f
          & Text.init -- Drop trailing dot, see notes on scoped_term_name_lookup schema
          & Text.splitOn "."
          & NonEmpty.fromList
          & into @ReversedName
    ref <- fromRow
    pure (NamedRef {reversedSegments, ref})

-- | The new 'scoped' name lookup format is different from the old version.
--
-- Specifically, the scoped format adds the 'lastNameSegment' as well as adding a trailing '.' to the db format
-- of both the namespace and reversed_name.
--
-- This type has a ToRow instance of the form:
-- [reversedName, namespace, lastNameSegment] <> ref fields...
newtype ScopedRow ref
  = ScopedRow (NamedRef ref)

instance ToRow ref => ToRow (ScopedRow ref) where
  toRow (ScopedRow (NamedRef {reversedSegments = revSegments, ref})) =
    SQLText reversedName : SQLText namespace : SQLText lastNameSegment : toRow ref
    where
      reversedName = (Text.intercalate "." . into @[Text] $ revSegments) <> "."
      namespace = (Text.intercalate "." . reverse . NEL.tail . from $ revSegments) <> "."
      lastNameSegment = NEL.head . from $ revSegments
