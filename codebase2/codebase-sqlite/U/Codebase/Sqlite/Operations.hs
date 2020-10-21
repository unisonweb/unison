{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module U.Codebase.Sqlite.Operations where

import Control.Monad ((>=>))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Functor ((<&>))
import qualified Data.Vector as Vector
import qualified U.Codebase.Decl as C
import qualified U.Codebase.Decl as C.Decl
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Sqlite.Decl.Format as S.Decl
import qualified U.Codebase.Sqlite.LocalIds as LocalIds
import U.Codebase.Sqlite.Queries (DB)
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Serialization as S
import U.Codebase.Sqlite.Symbol (Symbol)
import qualified U.Codebase.Sqlite.Term.Format as S.Term
import qualified U.Codebase.Term as C
import qualified U.Codebase.Term as C.Term
import qualified U.Codebase.Type as C.Type
import U.Util.Base32Hex (Base32Hex)
import qualified U.Util.Hash as H
import U.Util.Serialization (getFromBytes)
import U.Codebase.WatchKind (WatchKind)
import qualified U.Codebase.Reference as C
import qualified U.Codebase.Referent as C.Referent
import Data.Set (Set)
import U.Codebase.ShortHash (ShortBranchHash, ShortHash)

loadTermComponentByHash :: DB m => Base32Hex -> m (Maybe [C.Term Symbol])
loadTermComponentByHash = error "todo"

m :: (a -> f (Maybe b)) -> a -> MaybeT f b
m = fmap MaybeT

m' :: (Functor f, Show a) => String -> (a -> f (Maybe b)) -> a -> MaybeT f b
m' msg f a = MaybeT do
  f a <&> \case
    Nothing -> error $ "nothing: " ++ msg ++ " " ++ show a
    Just b -> Just b

loadTermByHash :: DB m => C.Reference.Id -> m (Maybe (C.Term Symbol))
loadTermByHash (C.Reference.Id h i) = runMaybeT do
  -- retrieve and deserialize the blob
  (localIds, term) <- do
    oId <- m' "Q.objectIdByAnyHash" (Q.objectIdByAnyHash . H.toBase32Hex) h
    bytes <- m' "Q.loadObjectById" Q.loadObjectById oId
    m'
      ("getTermElement: " ++ show i ++ ") fromBytes:")
      (fmap pure $ getFromBytes $ S.lookupTermElement i) bytes

  -- look up the text and hashes that are used by the term
  texts <- traverse (m' "Q.loadTextById" Q.loadTextById) $ LocalIds.textLookup localIds
  hashes <- traverse (m' "Q.loadPrimaryHashByObjectId" Q.loadPrimaryHashByObjectId) $ LocalIds.objectLookup localIds

  -- substitute the text and hashes back into the term
  let substText (S.Term.LocalTextId w) = texts Vector.! fromIntegral w
      substHash (S.Term.LocalDefnId w) = H.fromBase32Hex $ hashes Vector.! fromIntegral w
      substTermRef = bimap substText (fmap substHash)
      substTypeRef = bimap substText substHash
      substTermLink = bimap substTermRef substTypeRef
      substTypeLink = substTypeRef
  pure (C.Term.extraMap substText substTermRef substTypeRef substTermLink substTypeLink id term)

loadTypeOfTermByTermHash :: DB m => C.Reference.Id -> m (Maybe (C.Term.Type Symbol))
loadTypeOfTermByTermHash r = runMaybeT do
  -- convert query reference by looking up db ids
  let externalToDb =
        m' "Q.loadHashId" Q.loadHashId . H.toBase32Hex
          >=> m' "Q.objectIdByPrimaryHashId" Q.objectIdByPrimaryHashId
  r' <- C.Reference.idH externalToDb r
  -- load "type of term" blob for the reference
  bytes <- m' "Q.loadTypeOfTerm" Q.loadTypeOfTerm r'
  -- deserialize the blob into the type
  typ <- m' "getTypeFromBytes" (fmap pure $ getFromBytes $ S.getType S.getReference) bytes
  -- convert the result type by looking up db ids
  let dbToExternal =
        bitraverse
          (m Q.loadTextById)
          (fmap H.fromBase32Hex . m Q.loadPrimaryHashByObjectId)
  C.Type.rtraverse dbToExternal typ

loadDeclByHash :: DB m => C.Reference.Id -> m (Maybe (C.Decl Symbol))
loadDeclByHash (C.Reference.Id h i) = runMaybeT do
  -- retrieve the blob
  (localIds, C.Decl.DataDeclaration dt m b ct) <- do
    oId <- m' "Q.objectIdByAnyHash" (Q.objectIdByAnyHash . H.toBase32Hex) h
    bytes <- m' "Q.loadObjectById" Q.loadObjectById oId
    m' ("getDeclElement: " ++ show i ++ ") fromBytes:") (pure . getFromBytes (S.lookupDeclElement i)) bytes

  -- look up the text and hashes that are used by the term
  texts <- traverse (m' "Q.loadTextById" Q.loadTextById) $ LocalIds.textLookup localIds
  hashes <- traverse (m' "Q.loadPrimaryHashByObjectId" Q.loadPrimaryHashByObjectId) $ LocalIds.objectLookup localIds

  -- substitute the text and hashes back into the term
  let substText (S.Decl.LocalTextId w) = texts Vector.! fromIntegral w
      substHash (S.Decl.LocalTypeId w) = H.fromBase32Hex $ hashes Vector.! fromIntegral w
      substTypeRef :: S.Decl.TypeRef -> C.Decl.TypeRef
      substTypeRef = bimap substText (fmap substHash)
  pure (C.Decl.DataDeclaration dt m b (C.Type.rmap substTypeRef <$> ct)) -- lens might be nice here

saveTerm :: DB m => C.Reference.Id -> C.Term Symbol -> C.Term.Type Symbol -> m ()
saveTerm = error "todo"

saveDecl :: DB m => C.Reference.Id -> C.Decl Symbol -> m ()
saveDecl = error "todo"

listWatches :: DB m => WatchKind -> m [C.Reference.Id]
listWatches = error "todo"

loadWatch :: DB m => WatchKind -> C.Reference.Id -> m (Maybe (C.Term Symbol))
loadWatch = error "todo"

saveWatch :: DB m => WatchKind -> C.Reference.Id -> C.Term Symbol -> m ()
saveWatch = error "todo"

termsHavingType :: DB m => C.Reference -> m (Set C.Referent.Id)
termsHavingType = error "todo"

termsMentioningType :: DB m => C.Reference -> m (Set C.Referent.Id)
termsMentioningType = error "todo"

termReferencesByPrefix :: DB m => ShortHash -> m (Set C.Reference.Id)
termReferencesByPrefix = error "todo"

typeReferencesByPrefix :: DB m => ShortHash -> m (Set C.Reference.Id)
typeReferencesByPrefix = error "todo"

termReferentsByPrefix :: DB m => ShortHash -> m (Set C.Referent.Id)
termReferentsByPrefix = error "todo"

branchHashesByPrefix :: DB m => ShortBranchHash -> m (Set C.Reference.Id)
branchHashesByPrefix = error "todo"

dependents :: DB m => C.Reference -> m (Maybe (Set C.Reference.Id))
dependents = error "todo"

termDependencies :: DB m => C.Reference.Id -> m (Maybe (Set C.Reference.Id))
termDependencies = error "todo"

declDependencies :: DB m => C.Reference.Id -> m (Maybe (Set C.Reference.Id))
declDependencies = error "todo"


-- getBranchByAnyHash ::
-- getBranchByBranchHash :: DB m => BranchHash -> m (Maybe (Branch m))
-- getBranchByCausalHash :: DB m => CausalHash -> m (Maybe (Branch m))

-- lca              :: (forall he e. [Causal m CausalHash he e] -> m (Maybe BranchHash)),

-- branchDependencies ::
--   Branch.Hash -> m (Maybe (CausalHash, BD.Dependencies)),
-- -- |the "new" terms and types mentioned in a patch
-- patchDependencies :: EditHash -> m (Set Reference, Set Reference)
