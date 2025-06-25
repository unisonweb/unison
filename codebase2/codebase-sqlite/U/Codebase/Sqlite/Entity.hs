module U.Codebase.Sqlite.Entity where

import Data.Bifunctor.Tannen (Tannen)
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import U.Codebase.Sqlite.Branch.Format qualified as Namespace
import U.Codebase.Sqlite.Causal qualified as Causal
import U.Codebase.Sqlite.DbId (BranchHashId, BranchObjectId, CausalHashId, HashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.Decl.Format qualified as Decl
import U.Codebase.Sqlite.Patch.Format qualified as Patch
import U.Codebase.Sqlite.TempEntityType (TempEntityType (..))
import U.Codebase.Sqlite.Term.Format qualified as Term

-- |
-- data SyncEntity
--   = TC SyncTermFormat
--   | DC SyncDeclFormat
--   | N SyncBranchFormat
--   | P SyncPatchFormat
--   | C SyncCausalFormat
type SyncEntity =
  SyncEntity' Term.SyncTermFormat' Decl.SyncDeclFormat' TextId HashId ObjectId PatchObjectId BranchHashId BranchObjectId CausalHashId

-- Include the encoded bytestring, alongside the locally indexed component bifunctor.
type WithEncoded p = Tannen ((,) (Vector ByteString)) p

type DecodedSyncEntity =
  DecodedSyncEntityF (WithEncoded Term.LocallyIndexedComponent') (WithEncoded Decl.LocallyIndexedComponent')

type DecodedSyncEntityF tf df =
  SyncEntity' tf df TextId HashId ObjectId PatchObjectId BranchHashId BranchObjectId CausalHashId

data SyncEntity' tf df text hash defn patch branchh branch causal
  = TC (tf text defn)
  | DC (df text defn)
  | N (Namespace.SyncBranchFormat' branch text defn patch (branch, causal))
  | P (Patch.SyncPatchFormat' patch text hash defn)
  | C (Causal.SyncCausalFormat' causal branchh)
  deriving stock (Eq, Show)

-- | Natural transformation for the entity's term format container.
hoistTermFormat :: (Applicative m) => (tf text defn -> m (tf' text defn)) -> SyncEntity' tf df text hash defn patch branchh branch causal -> m (SyncEntity' tf' df text hash defn patch branchh branch causal)
hoistTermFormat f = \case
  TC t -> TC <$> (f t)
  DC d -> pure $ DC d
  N n -> pure $ N n
  P p -> pure $ P p
  C c -> pure $ C c

-- | Natural transformation for the entity's decl format container.
hoistDeclFormat :: (Applicative m) => (df text defn -> m (df' text defn)) -> SyncEntity' tf df text hash defn patch branchh branch causal -> m (SyncEntity' tf df' text hash defn patch branchh branch causal)
hoistDeclFormat f = \case
  TC t -> pure $ TC t
  DC d -> DC <$> (f d)
  N n -> pure $ N n
  P p -> pure $ P p
  C c -> pure $ C c

entityType :: SyncEntity' tf df text hash defn patch branchh branch causal -> TempEntityType
entityType = \case
  TC _ -> TermComponentType
  DC _ -> DeclComponentType
  N _ -> NamespaceType
  P _ -> PatchType
  C _ -> CausalType
