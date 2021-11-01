module U.Codebase.Sqlite.Branch.Format
  ( BranchFormat (..),
    BranchLocalIds (..),
    SyncBranchFormat (..),
    localToDbBranch,
    dbToLocalBranch,
    localToDbDiff,
    -- dbToLocalDiff,
  )
where

import Control.Lens (Lens', zoom, _1, _2, _3, _4)
import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Bitraversable (bitraverse)
import Data.Coerce (Coercible, coerce)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import U.Codebase.Sqlite.Branch.Diff (Diff, LocalDiff)
import qualified U.Codebase.Sqlite.Branch.Diff as Branch.Diff
import U.Codebase.Sqlite.Branch.Full (DbBranch, LocalBranch)
import qualified U.Codebase.Sqlite.Branch.Full as Branch.Full
import U.Codebase.Sqlite.DbId (BranchObjectId, CausalHashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds
  ( LocalBranchChildId (..),
    LocalDefnId (..),
    LocalPatchObjectId (..),
    LocalTextId (..),
  )
import U.Codebase.Sqlite.Reference (LocalReference, Reference)
import U.Codebase.Sqlite.Referent (LocalReferent, Referent)
import qualified U.Util.Map as Map
import qualified U.Util.Set as Set
import Unison.Prelude

-- | A 'BranchFormat' is a deserialized namespace object (@object.bytes@).
--
-- you can use the exact same `BranchLocalIds` when converting between `Full` and `Diff`
data BranchFormat
  = Full BranchLocalIds LocalBranch
  | Diff BranchObjectId BranchLocalIds LocalDiff
  deriving (Show)

-- | A 'BranchLocalIds' is a mapping between local ids (local to this object) encoded as offsets, and actual database ids.
--
-- For example, a @branchTextLookup@ vector of @[50, 74]@ means "local id 0 corresponds to database text id 50, and
-- local id 1 corresponds to database text id 74".
data BranchLocalIds = LocalIds
  { branchTextLookup :: Vector TextId,
    branchDefnLookup :: Vector ObjectId,
    branchPatchLookup :: Vector PatchObjectId,
    branchChildLookup :: Vector (BranchObjectId, CausalHashId)
  }
  deriving (Show)

data SyncBranchFormat
  = SyncFull BranchLocalIds ByteString
  | SyncDiff BranchObjectId BranchLocalIds ByteString

localToDbBranch :: BranchLocalIds -> LocalBranch -> DbBranch
localToDbBranch li =
  Branch.Full.quadmap (lookupBranchLocalText li) (lookupBranchLocalDefn li) (lookupBranchLocalPatch li) (lookupBranchLocalChild li)

type DbToLocalBranchState =
  ( Map TextId LocalTextId,
    Map ObjectId LocalDefnId,
    Map PatchObjectId LocalPatchObjectId,
    Map (BranchObjectId, CausalHashId) LocalBranchChildId
  )

dbToLocalBranch :: DbBranch -> (BranchLocalIds, LocalBranch)
dbToLocalBranch (Branch.Full.Branch terms types patches children) =
  let (localBranch, (localTexts, localDefns, localPatches, localChildren)) =
        (`State.runState` (mempty @DbToLocalBranchState)) do
          Branch.Full.Branch
            <$> Map.bitraverse (zoom _1 . localText) (Map.bitraverse (zoom _1_2 . localReferent) (zoom _1_2 . localMetadata)) terms
            <*> Map.bitraverse (zoom _1 . localText) (Map.bitraverse (zoom _1_2 . localReference) (zoom _1_2 . localMetadata)) types
            <*> Map.bitraverse (zoom _1 . localText) (zoom _3 . localPatch) patches
            <*> Map.bitraverse (zoom _1 . localText) (zoom _4 . localChild) children
      branchLocalIds :: BranchLocalIds
      branchLocalIds =
        LocalIds
          { branchTextLookup = Map.valuesVector (Map.swap localTexts),
            branchDefnLookup = Map.valuesVector (Map.swap localDefns),
            branchPatchLookup = Map.valuesVector (Map.swap localPatches),
            branchChildLookup = Map.valuesVector (Map.swap localChildren)
          }
   in (branchLocalIds, localBranch)
  where
    localChild ::
      (BranchObjectId, CausalHashId) ->
      State (Map (BranchObjectId, CausalHashId) LocalBranchChildId) LocalBranchChildId
    localChild = localize

    localDefn :: ObjectId -> State (Map ObjectId LocalDefnId) LocalDefnId
    localDefn = localize

    localMetadata :: Branch.Full.DbMetadataSet -> State (Map TextId LocalTextId, Map ObjectId LocalDefnId) Branch.Full.LocalMetadataSet
    localMetadata (Branch.Full.Inline vals) =
      Branch.Full.Inline <$> Set.traverse localReference vals

    localText :: TextId -> State (Map TextId LocalTextId) LocalTextId
    localText = localize

    localPatch :: PatchObjectId -> State (Map PatchObjectId LocalPatchObjectId) LocalPatchObjectId
    localPatch = localize

    localReference :: Reference -> State (Map TextId LocalTextId, Map ObjectId LocalDefnId) LocalReference
    localReference = bitraverse (zoom _1 . localText) (zoom _2 . localDefn)

    localReferent :: Referent -> State (Map TextId LocalTextId, Map ObjectId LocalDefnId) LocalReferent
    localReferent = bitraverse localReference localReference

localize :: (Coercible local Word64, Ord real) => real -> State (Map real local) local
localize real = do
  mapping <- State.get
  case Map.lookup real mapping of
    Nothing -> do
      let nextLocal = coerce @Word64 (fromIntegral (Map.size mapping))
      State.put $! Map.insert real nextLocal mapping
      pure nextLocal
    Just local -> pure local

_1_2 :: Lens' (a, b, c, d) (a, b)
_1_2 f (a0, b0, c, d) =
  (\(a, b) -> (a, b, c, d)) <$> f (a0, b0)

lookupBranchLocalText :: BranchLocalIds -> LocalTextId -> TextId
lookupBranchLocalText li (LocalTextId w) = branchTextLookup li Vector.! fromIntegral w

lookupBranchLocalDefn :: BranchLocalIds -> LocalDefnId -> ObjectId
lookupBranchLocalDefn li (LocalDefnId w) = branchDefnLookup li Vector.! fromIntegral w

lookupBranchLocalPatch :: BranchLocalIds -> LocalPatchObjectId -> PatchObjectId
lookupBranchLocalPatch li (LocalPatchObjectId w) = branchPatchLookup li Vector.! fromIntegral w

lookupBranchLocalChild :: BranchLocalIds -> LocalBranchChildId -> (BranchObjectId, CausalHashId)
lookupBranchLocalChild li (LocalBranchChildId w) = branchChildLookup li Vector.! fromIntegral w

localToDbDiff :: BranchLocalIds -> LocalDiff -> Diff
localToDbDiff li = Branch.Diff.quadmap (lookupBranchLocalText li) (lookupBranchLocalDefn li) (lookupBranchLocalPatch li) (lookupBranchLocalChild li)
