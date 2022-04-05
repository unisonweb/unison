module Unison.Codebase.SqliteCodebase.Migrations.MigrateSchema1To2.DbHelpers
  ( dbBranchHash,
    dbPatchHash,
  )
where

import qualified U.Codebase.Reference as S hiding (Reference)
import qualified U.Codebase.Reference as S.Reference
import qualified U.Codebase.Referent as S.Referent
import U.Codebase.Sqlite.Branch.Full (DbMetadataSet)
import qualified U.Codebase.Sqlite.Branch.Full as S
import qualified U.Codebase.Sqlite.Branch.Full as S.Branch.Full
import qualified U.Codebase.Sqlite.Branch.Full as S.MetadataSet
import qualified U.Codebase.Sqlite.DbId as Db
import qualified U.Codebase.Sqlite.Patch.Full as S
import qualified U.Codebase.Sqlite.Patch.TermEdit as S (TermEdit)
import qualified U.Codebase.Sqlite.Patch.TermEdit as S.TermEdit
import qualified U.Codebase.Sqlite.Patch.TypeEdit as S (TypeEdit)
import qualified U.Codebase.Sqlite.Patch.TypeEdit as S.TypeEdit
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Reference as S
import qualified U.Codebase.Sqlite.Referent as S
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import Unison.Hash (Hash)
import Unison.Hashing.V2.Branch (NameSegment (..))
import qualified Unison.Hashing.V2.Branch as Hashing.Branch
import qualified Unison.Hashing.V2.Patch as Hashing (Patch (..))
import qualified Unison.Hashing.V2.Patch as Hashing.Patch
import qualified Unison.Hashing.V2.Reference as Hashing (Reference)
import qualified Unison.Hashing.V2.Reference as Hashing.Reference
import qualified Unison.Hashing.V2.Referent as Hashing (Referent)
import qualified Unison.Hashing.V2.Referent as Hashing.Referent
import qualified Unison.Hashing.V2.TermEdit as Hashing (TermEdit)
import qualified Unison.Hashing.V2.TermEdit as Hashing.TermEdit
import qualified Unison.Hashing.V2.TypeEdit as Hashing (TypeEdit)
import qualified Unison.Hashing.V2.TypeEdit as Hashing.TypeEdit
import Unison.Prelude
import Unison.Sqlite (Transaction)
import qualified Unison.Util.Map as Map
import qualified Unison.Util.Set as Set

dbBranchHash :: S.DbBranch -> Transaction Hash
dbBranchHash (S.Branch.Full.Branch tms tps patches children) =
  fmap Hashing.Branch.hashBranch $
    Hashing.Branch.Raw
      <$> doTerms tms
      <*> doTypes tps
      <*> doPatches patches
      <*> doChildren children
  where
    doTerms ::
      Map Db.TextId (Map S.Referent S.DbMetadataSet) ->
      Transaction (Map NameSegment (Map Hashing.Referent Hashing.Branch.MdValues))
    doTerms =
      Map.bitraverse
        s2hNameSegment
        (Map.bitraverse s2hReferent s2hMetadataSet)

    doTypes ::
      Map Db.TextId (Map S.Reference S.DbMetadataSet) ->
      Transaction (Map NameSegment (Map Hashing.Reference Hashing.Branch.MdValues))
    doTypes =
      Map.bitraverse
        s2hNameSegment
        (Map.bitraverse s2hReference s2hMetadataSet)

    doPatches :: Map Db.TextId Db.PatchObjectId -> Transaction (Map NameSegment Hash)
    doPatches =
      Map.bitraverse s2hNameSegment (objectIdToPrimaryHash . Db.unPatchObjectId)

    doChildren :: Map Db.TextId (Db.BranchObjectId, Db.CausalHashId) -> Transaction (Map NameSegment Hash)
    doChildren =
      Map.bitraverse s2hNameSegment \(_boId, chId) -> causalHashIdToHash chId

dbPatchHash :: S.Patch -> Transaction Hash
dbPatchHash S.Patch {S.termEdits, S.typeEdits} =
  fmap Hashing.Patch.hashPatch $
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

s2hMetadataSet :: DbMetadataSet -> Transaction Hashing.Branch.MdValues
s2hMetadataSet = \case
  S.MetadataSet.Inline rs -> Hashing.Branch.MdValues <$> Set.traverse s2hReference rs

s2hNameSegment :: Db.TextId -> Transaction NameSegment
s2hNameSegment =
  fmap NameSegment . Q.expectText

s2hReferent :: S.Referent -> Transaction Hashing.Referent
s2hReferent = \case
  S.Referent.Ref r -> Hashing.Referent.Ref <$> s2hReference r
  S.Referent.Con r cid -> Hashing.Referent.Con <$> s2hReference r <*> pure (fromIntegral cid)

s2hReferentH :: S.ReferentH -> Transaction Hashing.Referent
s2hReferentH = \case
  S.Referent.Ref r -> Hashing.Referent.Ref <$> s2hReferenceH r
  S.Referent.Con r cid -> Hashing.Referent.Con <$> s2hReferenceH r <*> pure (fromIntegral cid)

s2hReference :: S.Reference -> Transaction Hashing.Reference
s2hReference = \case
  S.ReferenceBuiltin t -> Hashing.Reference.Builtin <$> Q.expectText t
  S.Reference.Derived h i -> Hashing.Reference.Derived <$> objectIdToPrimaryHash h <*> pure i

s2hReferenceH :: S.ReferenceH -> Transaction Hashing.Reference
s2hReferenceH = \case
  S.ReferenceBuiltin t -> Hashing.Reference.Builtin <$> Q.expectText t
  S.Reference.Derived h i -> Hashing.Reference.Derived <$> expectHash h <*> pure i

s2hTermEdit :: S.TermEdit -> Transaction Hashing.TermEdit
s2hTermEdit = \case
  S.TermEdit.Replace r _typing -> Hashing.TermEdit.Replace <$> s2hReferent r
  S.TermEdit.Deprecate -> pure Hashing.TermEdit.Deprecate

s2hTypeEdit :: S.TypeEdit -> Transaction Hashing.TypeEdit
s2hTypeEdit = \case
  S.TypeEdit.Replace r -> Hashing.TypeEdit.Replace <$> s2hReference r
  S.TypeEdit.Deprecate -> pure Hashing.TypeEdit.Deprecate

-- Mitchell: Do these variants of Q.* queries belong somewhere else? Or in Q perhaps?

causalHashIdToHash :: Db.CausalHashId -> Transaction Hash
causalHashIdToHash =
  fmap Cv.hash2to1 . Q.expectHash . Db.unCausalHashId

objectIdToPrimaryHash :: Db.ObjectId -> Transaction Hash
objectIdToPrimaryHash =
  fmap Cv.hash2to1 . Q.expectPrimaryHashByObjectId

expectHash :: Db.HashId -> Transaction Hash
expectHash =
  fmap Cv.hash2to1 . Q.expectHash
