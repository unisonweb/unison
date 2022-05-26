-- | Combinators or utilities shared by sync server AND client
module Unison.Sync.Common where

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
import qualified U.Codebase.Sqlite.TempEntity as Sqlite
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
