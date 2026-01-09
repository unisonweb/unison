module Unison.SyncV3.Utils (tempEntityDependencies, entityDependencies) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.Lens qualified as Lens
import U.Codebase.Sqlite.Entity qualified as Entity
import U.Codebase.Sqlite.TempEntity
import Unison.Hash32 (Hash32)
import Unison.SyncV3.Types
import Unison.Util.Servant.CBOR qualified as CBOR

tempEntityDependencies :: TempEntity -> Set (EntityKind, Hash32)
tempEntityDependencies entity = do
  let componentDeps = Lens.setOf Entity.defns_ entity
      patchDeps = Lens.setOf Entity.patches_ entity
      branchHashes = Lens.setOf Entity.branchHashes_ entity <> Lens.setOf Entity.branches_ entity
      causalHashes = Lens.setOf Entity.causalHashes_ entity
   in Set.unions
        [ Set.map (DefnComponentEntity,) componentDeps,
          Set.map (PatchEntity,) patchDeps,
          Set.map (NamespaceEntity,) branchHashes,
          Set.map (CausalEntity,) causalHashes
        ]

entityDependencies :: Entity hash text -> Set (EntityKind, Hash32)
entityDependencies Entity {entityData} = do
  case (CBOR.deserialiseOrFailCBORBytes $ entityData) of
    -- TODO: proper error handling
    Left err -> error $ show err
    Right tempEntity -> tempEntityDependencies tempEntity
