{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module U.Codebase.Sqlite.Orphans where

import Control.Applicative
import U.Codebase.Branch.Type (NamespaceStats (..))
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Referent as C.Referent
import qualified U.Codebase.Reflog as Reflog
import U.Codebase.Sqlite.DbId
import U.Codebase.WatchKind (WatchKind)
import qualified U.Codebase.WatchKind as WatchKind
import U.Util.Base32Hex
import qualified U.Util.Hash as Hash
import Unison.Prelude
import Unison.Sqlite

-- Newtype for avoiding orphan instances
newtype AsSqlite a = AsSqlite {fromSQLite :: a}
  deriving (Show)

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

deriving via Text instance ToField Base32Hex

deriving via Text instance FromField Base32Hex

instance ToField WatchKind where
  toField = \case
    WatchKind.RegularWatch -> SQLInteger 0
    WatchKind.TestWatch -> SQLInteger 1

instance FromField WatchKind where
  fromField =
    fromField @Int8 <&> fmap \case
      0 -> WatchKind.RegularWatch
      1 -> WatchKind.TestWatch
      tag -> error $ "Unknown WatchKind id " ++ show tag

instance ToRow NamespaceStats where
  toRow (NamespaceStats {numContainedTerms, numContainedTypes, numContainedPatches}) =
    toRow (numContainedTerms, numContainedTypes, numContainedPatches)

instance FromRow NamespaceStats where
  fromRow = do
    numContainedTerms <- field
    numContainedTypes <- field
    numContainedPatches <- field
    pure $ NamespaceStats {..}

instance ToRow (Reflog.Entry CausalHashId Text) where
  toRow (Reflog.Entry time fromRootCausalHash toRootCausalHash reason) =
    toRow (time, fromRootCausalHash, toRootCausalHash, reason)

instance FromRow (Reflog.Entry CausalHashId Text) where
  fromRow = do
    time <- field
    fromRootCausalHash <- field
    toRootCausalHash <- field
    reason <- field
    pure $ Reflog.Entry {..}
