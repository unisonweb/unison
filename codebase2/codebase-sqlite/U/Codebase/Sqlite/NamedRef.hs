module U.Codebase.Sqlite.NamedRef where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text qualified as Text
import U.Codebase.Sqlite.NameLookups (ReversedName)
import Unison.Prelude
import Unison.Sqlite

data ConstructorType
  = DataConstructor
  | EffectConstructor

instance FromField (ConstructorType) where
  fromField f =
    fromField @Int f >>= \case
      0 -> pure DataConstructor
      1 -> pure EffectConstructor
      _ -> fail "Invalid ConstructorType"

data NamedRef ref = NamedRef {reversedSegments :: ReversedName, ref :: ref}
  deriving stock (Show, Functor, Foldable, Traversable)

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
