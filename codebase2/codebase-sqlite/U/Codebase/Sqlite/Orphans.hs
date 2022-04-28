module U.Codebase.Sqlite.Orphans where

import Control.Applicative
import qualified U.Codebase.Decl as Decl
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Referent as C.Referent
import qualified U.Util.Hash as Hash
import Unison.ConstructorReference (GConstructorReference (ConstructorReference))
import Unison.ConstructorType (ConstructorType)
import qualified Unison.ConstructorType as CT
import Unison.Prelude
import qualified Unison.Reference as V1
import qualified Unison.Referent as V1
import Unison.Sqlite

-- Newtype for avoiding orphan instances
newtype AsSqlite a = AsSqlite {fromSQLite :: a}
  deriving (Show)

instance ToRow (AsSqlite V1.Referent) where
  toRow (AsSqlite ref) = case ref of
    V1.Ref ref' -> toRow (AsSqlite ref') <> [SQLNull {- conId -}, SQLNull {- conType -}]
    V1.Con (ConstructorReference ref' conId) conType -> toRow (AsSqlite ref') <> [toField conId, toField (AsSqlite conType)]

instance ToRow (AsSqlite V1.Reference) where
  toRow (AsSqlite ref) = case ref of
    V1.Builtin txt -> [toField txt] <> [SQLNull, SQLNull]
    V1.DerivedId (V1.Id h p) -> [SQLNull, toField (AsSqlite h), toField p]

instance ToField (AsSqlite ConstructorType) where
  toField (AsSqlite ct) = case ct of
    CT.Data -> (SQLInteger 0)
    CT.Effect -> (SQLInteger 1)

instance FromField (AsSqlite ConstructorType) where
  fromField f =
    fromField @Int f >>= \case
      0 -> pure (AsSqlite CT.Data)
      1 -> pure (AsSqlite CT.Effect)
      _ -> fail "Invalid ConstructorType"

instance FromRow (AsSqlite V1.Referent) where
  fromRow = do
    AsSqlite reference <- fromRow @(AsSqlite V1.Reference)
    mayConId <- field @(Maybe Decl.ConstructorId)
    mayConType <- field @(Maybe (AsSqlite ConstructorType))

    case (mayConId, mayConType) of
      (Nothing, Nothing) -> pure $ AsSqlite (V1.Ref reference)
      (Just conId, Just (AsSqlite conType)) -> pure $ AsSqlite (V1.Con (ConstructorReference reference conId) conType)
      _ -> error "Invalid V1.Referent in Sqlite.FromRow"

instance FromRow (AsSqlite V1.Reference) where
  fromRow = do
    liftA3 (,,) field field field >>= \case
      (Just builtin, Nothing, Nothing) -> pure . AsSqlite $ (V1.Builtin builtin)
      (Nothing, Just (AsSqlite hash), Just pos) -> pure . AsSqlite $ V1.DerivedId (V1.Id hash pos)
      p -> error $ "Invalid Reference parameters" <> show p

instance ToRow (AsSqlite C.Reference.Reference) where
  toRow (AsSqlite ref) = case ref of
    C.Reference.ReferenceBuiltin txt -> [SQLText txt, SQLNull, SQLNull]
    C.Reference.ReferenceDerived (C.Reference.Id h p) -> [SQLNull, toField $ Hash.toBase32HexText h, toField p]

instance ToRow (AsSqlite C.Referent.Referent) where
  toRow (AsSqlite ref) = case ref of
    C.Referent.Ref ref' -> toRow (AsSqlite ref') <> [SQLNull]
    C.Referent.Con ref' conId -> toRow (AsSqlite ref') <> [toField conId]

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
