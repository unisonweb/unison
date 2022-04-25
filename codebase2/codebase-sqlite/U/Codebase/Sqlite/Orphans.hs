module U.Codebase.Sqlite.Orphans where

import Control.Applicative
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Referent as C.Referent
import qualified U.Codebase.Sqlite.Name as S
import qualified U.Util.Hash as Hash
import Unison.Prelude
import Unison.Sqlite

-- Newtype for avoiding orphan instances
newtype AsSqlite a = AsSqlite {fromSQLite :: a}
  deriving (Show)

instance ToRow (AsSqlite ref) => ToRow (AsSqlite (S.Name ref)) where
  toRow (AsSqlite (S.Name {S.name, S.ref})) =
    [toField name] <> toRow (AsSqlite ref)

instance ToRow (AsSqlite C.Reference.Reference) where
  toRow (AsSqlite ref) = case ref of
    C.Reference.ReferenceBuiltin txt -> [SQLText txt, SQLNull, SQLNull]
    C.Reference.ReferenceDerived (C.Reference.Id h p) -> [SQLNull, toField $ Hash.toBase32HexText h, toField p]

instance ToRow (AsSqlite C.Referent.Referent) where
  toRow (AsSqlite ref) = case ref of
    C.Referent.Ref ref' -> toRow (AsSqlite ref') <> [SQLNull]
    C.Referent.Con ref' conId -> toRow (AsSqlite ref') <> [toField conId]

instance FromRow (AsSqlite ref) => FromRow (AsSqlite (S.Name ref)) where
  fromRow = do
    name <- field
    AsSqlite ref <- fromRow
    pure . AsSqlite $ S.Name {S.name = name, S.ref = ref}

instance FromRow (AsSqlite C.Referent.Referent) where
  fromRow = do
    AsSqlite reference <- fromRow
    field >>= \case
      Nothing -> pure $ AsSqlite (C.Referent.Ref reference)
      Just conId -> pure $ AsSqlite (C.Referent.Con reference conId)

instance FromRow (AsSqlite C.Reference.Reference) where
  fromRow = do
    liftA3 (,,) field field field >>= \case
      (Just builtin, Nothing, Nothing) -> pure . AsSqlite $ (C.Reference.ReferenceBuiltin builtin)
      (Nothing, Just (AsSqlite hash), Just pos) -> pure . AsSqlite $ C.Reference.ReferenceDerived (C.Reference.Id hash pos)
      p -> error $ "Invalid Reference parameters" <> show p

instance ToField (AsSqlite Hash.Hash) where
  toField (AsSqlite h) = toField (Hash.toBase32HexText h)

instance FromField (AsSqlite Hash.Hash) where
  fromField f =
    fromField @Text f <&> \txt ->
      AsSqlite $ (Hash.unsafeFromBase32HexText txt)

fromSqliteRows :: [AsSqlite x] -> [x]
fromSqliteRows = coerce
