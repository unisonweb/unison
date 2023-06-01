module U.Codebase.Sqlite.NamedRef where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NEL
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import Unison.Prelude
import Unison.Sqlite

-- | E.g. ("map" :| ["List", "base"])
type ReversedSegments = NonEmpty Text

data ConstructorType
  = DataConstructor
  | EffectConstructor

instance ToField (ConstructorType) where
  toField ct = case ct of
    DataConstructor -> (SQLInteger 0)
    EffectConstructor -> (SQLInteger 1)

instance FromField (ConstructorType) where
  fromField f =
    fromField @Int f >>= \case
      0 -> pure DataConstructor
      1 -> pure EffectConstructor
      _ -> fail "Invalid ConstructorType"

data NamedRef ref = NamedRef {reversedSegments :: ReversedSegments, ref :: ref}
  deriving stock (Show, Functor, Foldable, Traversable)

instance (ToRow ref) => ToRow (NamedRef ref) where
  toRow (NamedRef {reversedSegments = segments, ref}) =
    [toField reversedName] <> toRow ref
    where
      reversedName =
        segments
          & toList
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
      reversedName = (Text.intercalate "." . toList $ revSegments) <> "."
      namespace = (Text.intercalate "." . reverse . NEL.tail $ revSegments) <> "."
      lastNameSegment = NEL.head revSegments
