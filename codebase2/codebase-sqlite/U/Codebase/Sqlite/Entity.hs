module U.Codebase.Sqlite.Entity where

import Control.Lens
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
  SyncEntity' TextId HashId ObjectId PatchObjectId BranchHashId BranchObjectId CausalHashId

data SyncEntity' text hash defn patch branchh branch causal
  = TC (Term.SyncTermFormat' text defn)
  | DC (Decl.SyncDeclFormat' text defn)
  | N (Namespace.SyncBranchFormat' branch text defn patch (branch, causal))
  | P (Patch.SyncPatchFormat' patch text hash defn)
  | C (Causal.SyncCausalFormat' causal branchh)
  deriving stock (Eq, Show)

entityType :: SyncEntity' text hash defn patch branchh branch causal -> TempEntityType
entityType = \case
  TC _ -> TermComponentType
  DC _ -> DeclComponentType
  N _ -> NamespaceType
  P _ -> PatchType
  C _ -> CausalType

texts_ :: Traversal (SyncEntity' text hash defn patch branchh branch causal) (SyncEntity' text' hash defn patch branchh branch causal) text text'
texts_ f = \case
  TC tcf -> TC <$> Term.syncTermFormatTexts_ f tcf
  DC dcf -> DC <$> Decl.syncDeclFormatTexts_ f dcf
  N ncf -> N <$> Namespace.syncBranchFormatTexts_ f ncf
  P pcf -> P <$> Patch.syncPatchFormatTexts_ f pcf
  C ccf -> pure (C ccf)

hashes_ :: Traversal (SyncEntity' text hash defn patch branchh branch causal) (SyncEntity' text hash' defn patch branchh branch causal) hash hash'
hashes_ f = \case
  TC tcf -> pure (TC tcf)
  DC dcf -> pure (DC dcf)
  N ncf -> pure (N ncf)
  P pcf -> P <$> Patch.syncPatchFormatHashes_ f pcf
  C ccf -> pure (C ccf)

defns_ :: Traversal (SyncEntity' text hash defn patch branchh branch causal) (SyncEntity' text hash defn' patch branchh branch causal) defn defn'
defns_ f = \case
  TC tcf -> TC <$> Term.syncTermFormatDefns_ f tcf
  DC dcf -> DC <$> Decl.syncDeclFormatDefns_ f dcf
  N ncf -> N <$> Namespace.syncBranchFormatDefns_ f ncf
  P pcf -> P <$> Patch.syncPatchFormatDefns_ f pcf
  C ccf -> pure (C ccf)

patches_ :: Traversal (SyncEntity' text hash defn patch branchh branch causal) (SyncEntity' text hash defn patch' branchh branch causal) patch patch'
patches_ f = \case
  TC tcf -> pure (TC tcf)
  DC dcf -> pure (DC dcf)
  N ncf -> N <$> Namespace.syncBranchFormatPatches_ f ncf
  P pcf -> P <$> Patch.syncPatchFormatParents_ f pcf
  C ccf -> pure (C ccf)

branchHashes_ :: Traversal (SyncEntity' text hash defn patch branchh branch causal) (SyncEntity' text hash defn patch branchh' branch causal) branchh branchh'
branchHashes_ f = \case
  TC tcf -> pure (TC tcf)
  DC dcf -> pure (DC dcf)
  N ncf -> pure (N ncf)
  P pcf -> pure (P pcf)
  C ccf -> C <$> Causal.syncCausalFormatValueHash_ f ccf

branches_ :: Traversal (SyncEntity' text hash defn patch branchh branch causal) (SyncEntity' text hash defn patch branchh branch' causal) branch branch'
branches_ f = \case
  TC tcf -> pure (TC tcf)
  DC dcf -> pure (DC dcf)
  N ncf ->
    ( case ncf of
        Namespace.SyncFull li bytes -> Namespace.SyncFull <$> (li & Namespace.branchLocalIdsChildren_ . _1 %%~ f) <*> pure bytes
        Namespace.SyncDiff parent li bytes ->
          Namespace.SyncDiff
            <$> (f parent)
            <*> (li & Namespace.branchLocalIdsChildren_ . _1 %%~ f)
            <*> pure bytes
    )
      <&> N
  P pcf -> pure (P pcf)
  C ccf -> pure (C ccf)

causalHashes_ :: Traversal (SyncEntity' text hash defn patch branchh branch causal) (SyncEntity' text hash defn patch branchh branch causal') causal causal'
causalHashes_ f = \case
  TC tcf -> pure (TC tcf)
  DC dcf -> pure (DC dcf)
  N ncf ->
    ( case ncf of
        Namespace.SyncFull li bytes -> Namespace.SyncFull <$> (li & Namespace.branchLocalIdsChildren_ . _2 %%~ f) <*> pure bytes
        Namespace.SyncDiff parent li bytes ->
          Namespace.SyncDiff
            <$> (pure parent)
            <*> (li & Namespace.branchLocalIdsChildren_ . _2 %%~ f)
            <*> pure bytes
    )
      <&> N
  P pcf -> pure (P pcf)
  C ccf -> C <$> Causal.syncCausalFormatCausalHash_ f ccf
