module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema1To2.DbHelpers
  ( dbBranchHash,
    dbPatchHash,
    syncCausalHash,
  )
where

import qualified Data.Set as Set
import qualified Data.Vector as Vector
import U.Codebase.HashTags (BranchHash (..), CausalHash (..), PatchHash (..))
import qualified U.Codebase.Reference as S hiding (Reference)
import qualified U.Codebase.Reference as S.Reference
import qualified U.Codebase.Referent as S.Referent
import U.Codebase.Sqlite.Branch.Full (DbMetadataSet)
import qualified U.Codebase.Sqlite.Branch.Full as S
import qualified U.Codebase.Sqlite.Branch.Full as S.Branch.Full
import qualified U.Codebase.Sqlite.Branch.Full as S.MetadataSet
import qualified U.Codebase.Sqlite.Causal as S
import qualified U.Codebase.Sqlite.DbId as Db
import qualified U.Codebase.Sqlite.Patch.Full as S
import qualified U.Codebase.Sqlite.Patch.TermEdit as S (TermEdit)
import qualified U.Codebase.Sqlite.Patch.TermEdit as S.TermEdit
import qualified U.Codebase.Sqlite.Patch.TypeEdit as S (TypeEdit)
import qualified U.Codebase.Sqlite.Patch.TypeEdit as S.TypeEdit
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Reference as S
import qualified U.Codebase.Sqlite.Referent as S
import Unison.Hash (Hash)
import qualified Unison.Hashing.V2 as Hashing
import Unison.Prelude
import Unison.Sqlite (Transaction)
import qualified Unison.Util.Map as Map
import qualified Unison.Util.Set as Set

syncCausalHash :: S.SyncCausalFormat -> Transaction CausalHash
syncCausalHash S.SyncCausalFormat {valueHash = valueHashId, parents = parentChIds} = do
  fmap (CausalHash . Hashing.contentHash) $
    Hashing.Causal
      <$> coerce @(Transaction BranchHash) @(Transaction Hash) (Q.expectBranchHash valueHashId)
      <*> fmap (Set.fromList . coerce @[CausalHash] @[Hash] . Vector.toList) (traverse Q.expectCausalHash parentChIds)

dbBranchHash :: S.DbBranch -> Transaction BranchHash
dbBranchHash (S.Branch.Full.Branch tms tps patches children) =
  fmap (BranchHash . Hashing.contentHash) $
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
  fmap (PatchHash . Hashing.contentHash) $
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
