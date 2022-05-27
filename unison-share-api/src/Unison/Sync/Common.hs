-- | Combinators or utilities shared by sync server AND client
module Unison.Sync.Common
  ( expectEntity,
    entityToTempEntity,
    tempEntityToEntity,
  )
where

import qualified Control.Lens as Lens
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified U.Codebase.Sqlite.Branch.Format as NamespaceFormat
import qualified U.Codebase.Sqlite.Causal as Causal
import qualified U.Codebase.Sqlite.Decl.Format as DeclFormat
import qualified U.Codebase.Sqlite.Entity as Entity
import U.Codebase.Sqlite.LocalIds
import qualified U.Codebase.Sqlite.Patch.Format as PatchFormat
import qualified U.Codebase.Sqlite.Queries as Q
import U.Codebase.Sqlite.TempEntity (TempEntity)
import qualified U.Codebase.Sqlite.TempEntity as Sqlite
import qualified U.Codebase.Sqlite.TempEntity as TempEntity
import qualified U.Codebase.Sqlite.Term.Format as TermFormat
import U.Util.Base32Hex (Base32Hex)
import Unison.Prelude
import qualified Unison.Sqlite as Sqlite
import qualified Unison.Sync.Types as Share

-- | Read an entity out of the database that we know is in main storage.
expectEntity :: Share.Hash -> Sqlite.Transaction (Share.Entity Text Share.Hash Share.Hash)
expectEntity hash = do
  syncEntity <- Q.expectEntity (Share.toBase32Hex hash)
  tempEntity <- Q.syncToTempEntity syncEntity
  pure (tempEntityToEntity tempEntity)

-- | Convert an entity that came over the wire from Unison Share into an equivalent type that we can store in the
-- `temp_entity` table.
entityToTempEntity :: forall hash. (hash -> Base32Hex) -> Share.Entity Text Share.Hash hash -> TempEntity
entityToTempEntity toBase32Hex = \case
  Share.TC (Share.TermComponent terms) ->
    terms
      & Vector.fromList
      & Vector.map (Lens.over Lens._1 mungeLocalIds)
      & TermFormat.SyncLocallyIndexedComponent
      & TermFormat.SyncTerm
      & Entity.TC
  Share.DC (Share.DeclComponent decls) ->
    decls
      & Vector.fromList
      & Vector.map (Lens.over Lens._1 mungeLocalIds)
      & DeclFormat.SyncLocallyIndexedComponent
      & DeclFormat.SyncDecl
      & Entity.DC
  Share.P Share.Patch {textLookup, oldHashLookup, newHashLookup, bytes} ->
    Entity.P (PatchFormat.SyncFull (mungePatchLocalIds textLookup oldHashLookup newHashLookup) bytes)
  Share.PD Share.PatchDiff {parent, textLookup, oldHashLookup, newHashLookup, bytes} ->
    Entity.P
      ( PatchFormat.SyncDiff
          (toBase32Hex parent)
          (mungePatchLocalIds textLookup oldHashLookup newHashLookup)
          bytes
      )
  Share.N Share.Namespace {textLookup, defnLookup, patchLookup, childLookup, bytes} ->
    Entity.N (NamespaceFormat.SyncFull (mungeNamespaceLocalIds textLookup defnLookup patchLookup childLookup) bytes)
  Share.ND Share.NamespaceDiff {parent, textLookup, defnLookup, patchLookup, childLookup, bytes} ->
    Entity.N
      ( NamespaceFormat.SyncDiff
          (toBase32Hex parent)
          (mungeNamespaceLocalIds textLookup defnLookup patchLookup childLookup)
          bytes
      )
  Share.C Share.Causal {namespaceHash, parents} ->
    Entity.C
      Causal.SyncCausalFormat
        { valueHash = toBase32Hex namespaceHash,
          parents = Vector.fromList (map toBase32Hex (Set.toList parents))
        }
  where
    mungeLocalIds :: Share.LocalIds Text hash -> TempEntity.TempLocalIds
    mungeLocalIds Share.LocalIds {texts, hashes} =
      LocalIds
        { textLookup = Vector.fromList texts,
          defnLookup = Vector.map toBase32Hex (Vector.fromList hashes)
        }

    mungeNamespaceLocalIds ::
      [Text] ->
      [hash] ->
      [hash] ->
      [(hash, hash)] ->
      TempEntity.TempNamespaceLocalIds
    mungeNamespaceLocalIds textLookup defnLookup patchLookup childLookup =
      NamespaceFormat.LocalIds
        { branchTextLookup = Vector.fromList textLookup,
          branchDefnLookup = Vector.fromList (map toBase32Hex defnLookup),
          branchPatchLookup = Vector.fromList (map toBase32Hex patchLookup),
          branchChildLookup = Vector.fromList (map (\(x, y) -> (toBase32Hex x, toBase32Hex y)) childLookup)
        }

    mungePatchLocalIds :: [Text] -> [Share.Hash] -> [hash] -> TempEntity.TempPatchLocalIds
    mungePatchLocalIds textLookup oldHashLookup newHashLookup =
      PatchFormat.LocalIds
        { patchTextLookup = Vector.fromList textLookup,
          patchHashLookup = Vector.fromList (coerce @[Share.Hash] @[Base32Hex] oldHashLookup),
          patchDefnLookup = Vector.fromList (map toBase32Hex newHashLookup)
        }

tempEntityToEntity :: Sqlite.TempEntity -> Share.Entity Text Share.Hash Share.Hash
tempEntityToEntity = \case
  Entity.TC (TermFormat.SyncTerm (TermFormat.SyncLocallyIndexedComponent terms)) ->
    terms
      & Vector.map (Lens.over Lens._1 mungeLocalIds)
      & Vector.toList
      & Share.TermComponent
      & Share.TC
  Entity.DC (DeclFormat.SyncDecl (DeclFormat.SyncLocallyIndexedComponent decls)) ->
    decls
      & Vector.map (Lens.over Lens._1 mungeLocalIds)
      & Vector.toList
      & Share.DeclComponent
      & Share.DC
  Entity.P format ->
    case format of
      PatchFormat.SyncFull PatchFormat.LocalIds {patchTextLookup, patchHashLookup, patchDefnLookup} bytes ->
        Share.P
          Share.Patch
            { textLookup = Vector.toList patchTextLookup,
              oldHashLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) patchHashLookup),
              newHashLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) patchDefnLookup),
              bytes
            }
      PatchFormat.SyncDiff parent PatchFormat.LocalIds {patchTextLookup, patchHashLookup, patchDefnLookup} bytes ->
        Share.PD
          Share.PatchDiff
            { parent = Share.Hash parent,
              textLookup = Vector.toList patchTextLookup,
              oldHashLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) patchHashLookup),
              newHashLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) patchDefnLookup),
              bytes
            }
  Entity.N format ->
    case format of
      NamespaceFormat.SyncFull
        NamespaceFormat.LocalIds
          { branchTextLookup,
            branchDefnLookup,
            branchPatchLookup,
            branchChildLookup
          }
        bytes ->
          Share.N
            Share.Namespace
              { textLookup = Vector.toList branchTextLookup,
                defnLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) branchDefnLookup),
                patchLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) branchPatchLookup),
                childLookup =
                  Vector.toList
                    (coerce @(Vector (Base32Hex, Base32Hex)) @(Vector (Share.Hash, Share.Hash)) branchChildLookup),
                bytes
              }
      NamespaceFormat.SyncDiff
        parent
        NamespaceFormat.LocalIds
          { branchTextLookup,
            branchDefnLookup,
            branchPatchLookup,
            branchChildLookup
          }
        bytes ->
          Share.ND
            Share.NamespaceDiff
              { parent = Share.Hash parent,
                textLookup = Vector.toList branchTextLookup,
                defnLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) branchDefnLookup),
                patchLookup = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) branchPatchLookup),
                childLookup =
                  Vector.toList
                    (coerce @(Vector (Base32Hex, Base32Hex)) @(Vector (Share.Hash, Share.Hash)) branchChildLookup),
                bytes
              }
  Entity.C Causal.SyncCausalFormat {valueHash, parents} ->
    Share.C
      Share.Causal
        { namespaceHash = Share.Hash valueHash,
          parents = Set.fromList (coerce @[Base32Hex] @[Share.Hash] (Vector.toList parents))
        }
  where
    mungeLocalIds :: LocalIds' Text Base32Hex -> Share.LocalIds Text Share.Hash
    mungeLocalIds LocalIds {textLookup, defnLookup} =
      Share.LocalIds
        { texts = Vector.toList textLookup,
          hashes = Vector.toList (coerce @(Vector Base32Hex) @(Vector Share.Hash) defnLookup)
        }
