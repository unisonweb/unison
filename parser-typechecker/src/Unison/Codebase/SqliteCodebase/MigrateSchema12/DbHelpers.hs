module Unison.Codebase.SqliteCodebase.MigrateSchema12.DbHelpers
  ( dbBranchHash,
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
import U.Codebase.Sqlite.Queries (EDB)
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Reference as S
import qualified U.Codebase.Sqlite.Referent as S
import qualified U.Util.Hash
import qualified U.Util.Map as Map
import qualified U.Util.Set as Set
import qualified Unison.Codebase.SqliteCodebase.Conversions as Cv
import Unison.Hash (Hash)
import qualified Unison.Hashable as H
import qualified Unison.Hashing.V2.Branch as Hashing.Branch
import qualified Unison.Hashing.V2.Reference as Hashing (Reference)
import qualified Unison.Hashing.V2.Reference as Hashing.Reference
import qualified Unison.Hashing.V2.Referent as Hashing (Referent)
import qualified Unison.Hashing.V2.Referent as Hashing.Referent
import Unison.NameSegment (NameSegment (..))
import Unison.Prelude

dbBranchHash :: EDB m => S.DbBranch -> m Hash
dbBranchHash (S.Branch.Full.Branch tms tps patches children) =
  fmap H.accumulate' $
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

    doTypes :: EDB m => Map Db.TextId (Map S.Reference S.DbMetadataSet) -> m (Map NameSegment (Map Hashing.Reference Hashing.Branch.MdValues))
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

s2hReference :: EDB m => S.Reference -> m Hashing.Reference
s2hReference = \case
  S.ReferenceBuiltin t -> Hashing.Reference.Builtin <$> Q.loadTextById t
  S.Reference.Derived h i -> Hashing.Reference.Derived <$> objectIdToPrimaryHash h <*> pure i

-- Mitchell: Do these variants of Q.* queries belong somewhere else? Or in Q perhaps?

objectIdToPrimaryHash :: EDB m => Db.ObjectId -> m Hash
objectIdToPrimaryHash =
  fmap (Cv.hash2to1 . U.Util.Hash.fromBase32Hex) . Q.loadPrimaryHashByObjectId

causalHashIdToHash :: EDB m => Db.CausalHashId -> m Hash
causalHashIdToHash =
  fmap (Cv.hash2to1 . U.Util.Hash.fromBase32Hex) . Q.loadHashById . Db.unCausalHashId
