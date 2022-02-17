module Unison.Codebase.SqliteCodebase.MigrateSchema12.DbHelpers
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
import U.Codebase.Sqlite.Queries (EDB)
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Reference as S
import qualified U.Codebase.Sqlite.Referent as S
import qualified U.Util.Hash
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
import qualified Unison.Util.Map as Map
import qualified Unison.Util.Set as Set

dbBranchHash :: EDB m => S.DbBranch -> m Hash
dbBranchHash (S.Branch.Full.Branch tms tps patches children) =
  fmap Hashing.Branch.hashBranch $
    Hashing.Branch.Raw
      <$> doTerms tms
      <*> doTypes tps
      <*> doPatches patches
      <*> doChildren children
  where
    doTerms :: EDB m => Map Db.TextId (Map S.Referent S.DbMetadataSet) -> m (Map NameSegment (Map Hashing.Referent Hashing.Branch.MdValues))
    doTerms =
      Map.bitraverse
        s2hNameSegment
        (Map.bitraverse s2hReferent s2hMetadataSet)

    doTypes ::
      EDB m =>
      Map Db.TextId (Map S.Reference S.DbMetadataSet) ->
      m (Map NameSegment (Map Hashing.Reference Hashing.Branch.MdValues))
    doTypes =
      Map.bitraverse
        s2hNameSegment
        (Map.bitraverse s2hReference s2hMetadataSet)

    doPatches :: EDB m => Map Db.TextId Db.PatchObjectId -> m (Map NameSegment Hash)
    doPatches =
      Map.bitraverse s2hNameSegment (objectIdToPrimaryHash . Db.unPatchObjectId)

    doChildren :: EDB m => Map Db.TextId (Db.BranchObjectId, Db.CausalHashId) -> m (Map NameSegment Hash)
    doChildren =
      Map.bitraverse s2hNameSegment \(_boId, chId) -> causalHashIdToHash chId

dbPatchHash :: forall m. EDB m => S.Patch -> m Hash
dbPatchHash S.Patch {S.termEdits, S.typeEdits} =
  fmap Hashing.Patch.hashPatch $
    Hashing.Patch
      <$> doTermEdits termEdits
      <*> doTypeEdits typeEdits
  where
    doTermEdits :: Map S.ReferentH (Set S.TermEdit) -> m (Map Hashing.Referent (Set Hashing.TermEdit))
    doTermEdits =
      Map.bitraverse s2hReferentH (Set.traverse s2hTermEdit)

    doTypeEdits :: Map S.ReferenceH (Set S.TypeEdit) -> m (Map Hashing.Reference (Set Hashing.TypeEdit))
    doTypeEdits =
      Map.bitraverse s2hReferenceH (Set.traverse s2hTypeEdit)

s2hMetadataSet :: EDB m => DbMetadataSet -> m Hashing.Branch.MdValues
s2hMetadataSet = \case
  S.MetadataSet.Inline rs -> Hashing.Branch.MdValues <$> Set.traverse s2hReference rs

s2hNameSegment :: EDB m => Db.TextId -> m NameSegment
s2hNameSegment =
  fmap NameSegment . Q.loadTextById

s2hReferent :: EDB m => S.Referent -> m Hashing.Referent
s2hReferent = \case
  S.Referent.Ref r -> Hashing.Referent.Ref <$> s2hReference r
  S.Referent.Con r cid -> Hashing.Referent.Con <$> s2hReference r <*> pure (fromIntegral cid)

s2hReferentH :: EDB m => S.ReferentH -> m Hashing.Referent
s2hReferentH = \case
  S.Referent.Ref r -> Hashing.Referent.Ref <$> s2hReferenceH r
  S.Referent.Con r cid -> Hashing.Referent.Con <$> s2hReferenceH r <*> pure (fromIntegral cid)

s2hReference :: EDB m => S.Reference -> m Hashing.Reference
s2hReference = \case
  S.ReferenceBuiltin t -> Hashing.Reference.Builtin <$> Q.loadTextById t
  S.Reference.Derived h i -> Hashing.Reference.Derived <$> objectIdToPrimaryHash h <*> pure i

s2hReferenceH :: EDB m => S.ReferenceH -> m Hashing.Reference
s2hReferenceH = \case
  S.ReferenceBuiltin t -> Hashing.Reference.Builtin <$> Q.loadTextById t
  S.Reference.Derived h i -> Hashing.Reference.Derived <$> loadHashHashById h <*> pure i

s2hTermEdit :: EDB m => S.TermEdit -> m Hashing.TermEdit
s2hTermEdit = \case
  S.TermEdit.Replace r _typing -> Hashing.TermEdit.Replace <$> s2hReferent r
  S.TermEdit.Deprecate -> pure Hashing.TermEdit.Deprecate

s2hTypeEdit :: EDB m => S.TypeEdit -> m Hashing.TypeEdit
s2hTypeEdit = \case
  S.TypeEdit.Replace r -> Hashing.TypeEdit.Replace <$> s2hReference r
  S.TypeEdit.Deprecate -> pure Hashing.TypeEdit.Deprecate

-- Mitchell: Do these variants of Q.* queries belong somewhere else? Or in Q perhaps?

causalHashIdToHash :: EDB m => Db.CausalHashId -> m Hash
causalHashIdToHash =
  fmap Cv.hash2to1 . Q.loadHashHashById . Db.unCausalHashId

objectIdToPrimaryHash :: EDB m => Db.ObjectId -> m Hash
objectIdToPrimaryHash =
  fmap (Cv.hash2to1 . U.Util.Hash.fromBase32Hex) . Q.loadPrimaryHashByObjectId

loadHashHashById :: EDB m => Db.HashId -> m Hash
loadHashHashById =
  fmap Cv.hash2to1 . Q.loadHashHashById
