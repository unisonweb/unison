module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema1To2.DbHelpers
  ( dbBranchHash,
    dbPatchHash,
    syncCausalHash,
  )
where

import Data.Set qualified as Set
import Data.Vector qualified as Vector
import U.Codebase.HashTags (BranchHash (..), CausalHash (..), PatchHash (..))
import U.Codebase.Reference qualified as S hiding (Reference)
import U.Codebase.Reference qualified as S.Reference
import U.Codebase.Referent qualified as S.Referent
import U.Codebase.Sqlite.Branch.Full (DbMetadataSet)
import U.Codebase.Sqlite.Branch.Full qualified as S
import U.Codebase.Sqlite.Branch.Full qualified as S.Branch.Full
import U.Codebase.Sqlite.Branch.Full qualified as S.MetadataSet
import U.Codebase.Sqlite.Causal qualified as S
import U.Codebase.Sqlite.DbId qualified as Db
import U.Codebase.Sqlite.Patch.Full qualified as S
import U.Codebase.Sqlite.Patch.TermEdit qualified as S (TermEdit)
import U.Codebase.Sqlite.Patch.TermEdit qualified as S.TermEdit
import U.Codebase.Sqlite.Patch.TypeEdit qualified as S (TypeEdit)
import U.Codebase.Sqlite.Patch.TypeEdit qualified as S.TypeEdit
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Reference qualified as S
import U.Codebase.Sqlite.Referent qualified as S
import Unison.Hash (Hash)
import Unison.Hash32 qualified as Hash32
import Unison.Hashing.V2 qualified as Hashing
import Unison.Prelude
import Unison.Sqlite (Transaction)
import Unison.Util.Map qualified as Map
import Unison.Util.Set qualified as Set

syncCausalHash :: S.SyncCausalFormat -> Transaction CausalHash
syncCausalHash S.SyncCausalFormat {valueHash = valueHashId, parents = parentChIds} = do
  fmap (CausalHash . Hash32.fromHash . Hashing.contentHash) $
    Hashing.Causal
      <$> fmap (Hash32.toHash . unBranchHash) (Q.expectBranchHash valueHashId)
      <*> fmap (Set.fromList . fmap (Hash32.toHash . unCausalHash) . Vector.toList) (traverse Q.expectCausalHash parentChIds)

dbBranchHash :: S.DbBranch -> Transaction BranchHash
dbBranchHash (S.Branch.Full.Branch tms tps patches children) =
  fmap (BranchHash . Hash32.fromHash . Hashing.contentHash) $
    Hashing.Branch
      <$> doTerms tms
      <*> doTypes tps
      <*> doPatches patches
      <*> doChildren children
  where
    doTerms ::
      Map Db.TextId (Map S.Referent S.DbMetadataSet) ->
      Transaction (Map Hashing.NameSegment (Map Hashing.Referent Hashing.MdValues))
    doTerms =
      Map.bitraverse
        s2hNameSegment
        (Map.bitraverse s2hReferent s2hMetadataSet)

    doTypes ::
      Map Db.TextId (Map S.Reference S.DbMetadataSet) ->
      Transaction (Map Hashing.NameSegment (Map Hashing.Reference Hashing.MdValues))
    doTypes =
      Map.bitraverse
        s2hNameSegment
        (Map.bitraverse s2hReference s2hMetadataSet)

    doPatches :: Map Db.TextId Db.PatchObjectId -> Transaction (Map Hashing.NameSegment Hash)
    doPatches =
      Map.bitraverse s2hNameSegment (Q.expectPrimaryHashByObjectId . Db.unPatchObjectId)

    doChildren :: Map Db.TextId (Db.BranchObjectId, Db.CausalHashId) -> Transaction (Map Hashing.NameSegment Hash)
    doChildren =
      Map.bitraverse s2hNameSegment \(_boId, chId) -> Q.expectHash (Db.unCausalHashId chId)

dbPatchHash :: S.Patch -> Transaction PatchHash
dbPatchHash S.Patch {S.termEdits, S.typeEdits} =
  fmap (PatchHash . Hash32.fromHash . Hashing.contentHash) $
    Hashing.Patch
      <$> doTermEdits termEdits
      <*> doTypeEdits typeEdits
  where
    doTermEdits :: Map S.ReferentH (Set S.TermEdit) -> Transaction (Map Hashing.Referent (Set Hashing.TermEdit))
    doTermEdits =
      Map.bitraverse s2hReferentH (Set.traverse s2hTermEdit)

    doTypeEdits :: Map S.ReferenceH (Set S.TypeEdit) -> Transaction (Map Hashing.Reference (Set Hashing.TypeEdit))
    doTypeEdits =
      Map.bitraverse s2hReferenceH (Set.traverse s2hTypeEdit)

s2hMetadataSet :: DbMetadataSet -> Transaction Hashing.MdValues
s2hMetadataSet = \case
  S.MetadataSet.Inline rs -> Hashing.MdValues <$> Set.traverse s2hReference rs

s2hNameSegment :: Db.TextId -> Transaction Hashing.NameSegment
s2hNameSegment =
  fmap Hashing.NameSegment . Q.expectText

s2hReferent :: S.Referent -> Transaction Hashing.Referent
s2hReferent = \case
  S.Referent.Ref r -> Hashing.ReferentRef <$> s2hReference r
  S.Referent.Con r cid -> Hashing.ReferentCon <$> s2hReference r <*> pure (fromIntegral cid)

s2hReferentH :: S.ReferentH -> Transaction Hashing.Referent
s2hReferentH = \case
  S.Referent.Ref r -> Hashing.ReferentRef <$> s2hReferenceH r
  S.Referent.Con r cid -> Hashing.ReferentCon <$> s2hReferenceH r <*> pure (fromIntegral cid)

s2hReference :: S.Reference -> Transaction Hashing.Reference
s2hReference = \case
  S.ReferenceBuiltin t -> Hashing.ReferenceBuiltin <$> Q.expectText t
  S.Reference.Derived h i -> Hashing.ReferenceDerived <$> Q.expectPrimaryHashByObjectId h <*> pure i

s2hReferenceH :: S.ReferenceH -> Transaction Hashing.Reference
s2hReferenceH = \case
  S.ReferenceBuiltin t -> Hashing.ReferenceBuiltin <$> Q.expectText t
  S.Reference.Derived h i -> Hashing.ReferenceDerived <$> Q.expectHash h <*> pure i

s2hTermEdit :: S.TermEdit -> Transaction Hashing.TermEdit
s2hTermEdit = \case
  S.TermEdit.Replace r _typing -> Hashing.TermEditReplace <$> s2hReferent r
  S.TermEdit.Deprecate -> pure Hashing.TermEditDeprecate

s2hTypeEdit :: S.TypeEdit -> Transaction Hashing.TypeEdit
s2hTypeEdit = \case
  S.TypeEdit.Replace r -> Hashing.TypeEditReplace <$> s2hReference r
  S.TypeEdit.Deprecate -> pure Hashing.TypeEditDeprecate
