{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module U.Codebase.Sqlite.Orphans where

import U.Codebase.Branch.Type (NamespaceStats (..))
import U.Codebase.Reflog qualified as Reflog
import U.Codebase.Sqlite.DbId
import U.Codebase.WatchKind (WatchKind)
import U.Codebase.WatchKind qualified as WatchKind
import U.Util.Base32Hex
import Unison.Prelude
import Unison.Sqlite

deriving via Text instance ToField Base32Hex

deriving via Text instance FromField Base32Hex

instance ToField WatchKind where
  toField = \case
    WatchKind.RegularWatch -> SQLInteger 0
    WatchKind.TestWatch -> SQLInteger 1

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
