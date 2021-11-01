module U.Codebase.Sqlite.Patch.Format
  ( PatchFormat (..),
    PatchLocalIds (..),
    SyncPatchFormat (..),
    localPatchToPatch,
    patchToLocalPatch,
  )
where

import Control.Lens (Lens', zoom, _1, _2)
import Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Bitraversable (bitraverse)
import Data.Coerce (Coercible, coerce)
import qualified Data.Map.Strict as Map
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import U.Codebase.Sqlite.DbId (HashId, ObjectId, PatchObjectId, TextId)
import U.Codebase.Sqlite.LocalIds (LocalDefnId (LocalDefnId), LocalHashId (LocalHashId), LocalTextId (LocalTextId))
import U.Codebase.Sqlite.Patch.Diff (LocalPatchDiff)
import U.Codebase.Sqlite.Patch.Full (LocalPatch, Patch)
import qualified U.Codebase.Sqlite.Patch.Full as Patch.Full
import U.Codebase.Sqlite.Patch.TermEdit (LocalTermEdit, TermEdit)
import U.Codebase.Sqlite.Patch.TypeEdit (LocalTypeEdit, TypeEdit)
import U.Codebase.Sqlite.Reference (LocalReferenceH, ReferenceH)
import U.Codebase.Sqlite.Referent (LocalReferentH, ReferentH)
import qualified U.Util.Map as Map
import qualified U.Util.Set as Set
import Unison.Prelude

data PatchFormat
  = Full PatchLocalIds LocalPatch
  | Diff PatchObjectId PatchLocalIds LocalPatchDiff

data PatchLocalIds = LocalIds
  { patchTextLookup :: Vector TextId,
    patchHashLookup :: Vector HashId,
    patchDefnLookup :: Vector ObjectId
  }

data SyncPatchFormat
  = SyncFull PatchLocalIds ByteString
  | SyncDiff PatchObjectId PatchLocalIds ByteString

localPatchToPatch :: PatchLocalIds -> LocalPatch -> Patch
localPatchToPatch li =
  Patch.Full.trimap (lookupPatchLocalText li) (lookupPatchLocalHash li) (lookupPatchLocalDefn li)
  where
    lookupPatchLocalText :: PatchLocalIds -> LocalTextId -> TextId
    lookupPatchLocalText li (LocalTextId w) = patchTextLookup li Vector.! fromIntegral w

    lookupPatchLocalHash :: PatchLocalIds -> LocalHashId -> HashId
    lookupPatchLocalHash li (LocalHashId w) = patchHashLookup li Vector.! fromIntegral w

    lookupPatchLocalDefn :: PatchLocalIds -> LocalDefnId -> ObjectId
    lookupPatchLocalDefn li (LocalDefnId w) = patchDefnLookup li Vector.! fromIntegral w

type PatchToLocalPatchState =
  ( Map TextId LocalTextId,
    Map HashId LocalHashId,
    Map ObjectId LocalDefnId
  )

patchToLocalPatch :: Patch -> (PatchLocalIds, LocalPatch)
patchToLocalPatch (Patch.Full.Patch termEdits typeEdits) =
  let (localPatch, (localTexts, localHashes, localDefns)) =
        (`State.runState` (mempty @PatchToLocalPatchState)) do
          Patch.Full.Patch
            <$> Map.bitraverse (zoom _1_2 . localReferentH) (Set.traverse (zoom _1_3 . localTermEdit)) termEdits
            <*> Map.bitraverse (zoom _1_2 . localReferenceH) (Set.traverse (zoom _1_3 . localTypeEdit)) typeEdits
      patchLocalIds :: PatchLocalIds
      patchLocalIds =
        LocalIds
          { patchTextLookup = Map.valuesVector (Map.swap localTexts),
            patchHashLookup = Map.valuesVector (Map.swap localHashes),
            patchDefnLookup = Map.valuesVector (Map.swap localDefns)
          }
   in (patchLocalIds, localPatch)
  where
    localDefn :: ObjectId -> State (Map ObjectId LocalDefnId) LocalDefnId
    localDefn = localize

    localText :: TextId -> State (Map TextId LocalTextId) LocalTextId
    localText = localize

    localHash :: HashId -> State (Map HashId LocalHashId) LocalHashId
    localHash = localize

    localReferenceH :: ReferenceH -> State (Map TextId LocalTextId, Map HashId LocalHashId) LocalReferenceH
    localReferenceH = bitraverse (zoom _1 . localText) (zoom _2 . localHash)

    localReferentH :: ReferentH -> State (Map TextId LocalTextId, Map HashId LocalHashId) LocalReferentH
    localReferentH = bitraverse localReferenceH localReferenceH

    localTermEdit :: TermEdit -> State (Map TextId LocalTextId, Map ObjectId LocalDefnId) LocalTermEdit
    localTermEdit = bitraverse (zoom _1 . localText) (zoom _2 . localDefn)

    localTypeEdit :: TypeEdit -> State (Map TextId LocalTextId, Map ObjectId LocalDefnId) LocalTypeEdit
    localTypeEdit = bitraverse (zoom _1 . localText) (zoom _2 . localDefn)

localize :: (Coercible local Word64, Ord real) => real -> State (Map real local) local
localize real = do
  mapping <- State.get
  case Map.lookup real mapping of
    Nothing -> do
      let nextLocal = coerce @Word64 (fromIntegral (Map.size mapping))
      State.put $! Map.insert real nextLocal mapping
      pure nextLocal
    Just local -> pure local

_1_2 :: Lens' (a, b, c) (a, b)
_1_2 f (a0, b0, c) =
  (\(a, b) -> (a, b, c)) <$> f (a0, b0)

_1_3 :: Lens' (a, b, c) (a, c)
_1_3 f (a0, b, c0) =
  (\(a, c) -> (a, b, c)) <$> f (a0, c0)
