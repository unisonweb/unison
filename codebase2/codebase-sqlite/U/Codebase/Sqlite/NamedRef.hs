module U.Codebase.Sqlite.NamedRef where

import qualified Data.List.NonEmpty as NEL
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text
import U.Codebase.Sqlite.NameLookups (ReversedName)
import Unison.Prelude
import Unison.Sqlite

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
-- Converts a NamedRef to SQLData of the form:
-- [reversedName, namespace, lastNameSegment] <> ref fields...
namedRefToScopedRow :: (ToRow ref) => NamedRef ref -> [SQLData]
namedRefToScopedRow (NamedRef {reversedSegments = revSegments, ref}) =
  toRow $ (SQLText reversedName, SQLText namespace, SQLText lastNameSegment) :. ref
  where
    reversedName = (Text.intercalate "." . into @[Text] $ revSegments) <> "."
    namespace = (Text.intercalate "." . reverse . NEL.tail . from $ revSegments) <> "."
    lastNameSegment = NEL.head . from $ revSegments
