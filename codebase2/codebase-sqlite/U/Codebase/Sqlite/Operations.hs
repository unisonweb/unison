{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module U.Codebase.Sqlite.Operations where

import Control.Monad ((>=>))
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as Vector
import Data.Word (Word64)
import qualified U.Codebase.Decl as C
import qualified U.Codebase.Decl as C.Decl
import qualified U.Codebase.Reference as C
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Referent as C.Referent
import U.Codebase.ShortHash (ShortBranchHash, ShortHash)
import qualified U.Codebase.Sqlite.DbId as Db
import qualified U.Codebase.Sqlite.Decl.Format as S.Decl
import U.Codebase.Sqlite.LocalIds (LocalIds)
import qualified U.Codebase.Sqlite.LocalIds as LocalIds
import U.Codebase.Sqlite.Queries (DB)
import qualified U.Codebase.Sqlite.Queries as Q
import qualified U.Codebase.Sqlite.Reference as S
import qualified U.Codebase.Sqlite.Reference as S.Reference
import qualified U.Codebase.Sqlite.Serialization as S
import U.Codebase.Sqlite.Symbol (Symbol)
import qualified U.Codebase.Sqlite.Term.Format as S.Term
import qualified U.Codebase.Term as C
import qualified U.Codebase.Term as C.Term
import qualified U.Codebase.Type as C.Type
import U.Codebase.WatchKind (WatchKind)
import U.Util.Base32Hex (Base32Hex)
import qualified U.Util.Hash as H
import U.Util.Serialization (getFromBytes)
import qualified U.Util.Serialization as S

loadTermComponentByHash :: DB m => Base32Hex -> m (Maybe [C.Term Symbol])
loadTermComponentByHash = error "todo"

-- * helpers

m :: (a -> f (Maybe b)) -> a -> MaybeT f b
m = fmap MaybeT

m' :: (Functor f, Show a) => String -> (a -> f (Maybe b)) -> a -> MaybeT f b
m' msg f a = MaybeT do
  f a <&> \case
    Nothing -> error $ "nothing: " ++ msg ++ " " ++ show a
    Just b -> Just b

c2sReference :: DB m => C.Reference -> MaybeT m S.Reference
c2sReference = bitraverse lookupTextId hashToObjectId

s2cReference :: DB m => S.Reference -> MaybeT m C.Reference
s2cReference = bitraverse loadTextById loadHashByObjectId

c2sReferenceId :: DB m => C.Reference.Id -> MaybeT m S.Reference.Id
c2sReferenceId = C.Reference.idH hashToObjectId

s2cReferenceId :: DB m => S.Reference.Id -> MaybeT m C.Reference.Id
s2cReferenceId = C.Reference.idH loadHashByObjectId

lookupTextId :: DB m => Text -> MaybeT m Db.TextId
lookupTextId = m' "Q.loadText" Q.loadText

loadTextById :: DB m => Db.TextId -> MaybeT m Text
loadTextById = m' "Q.loadTextById" Q.loadTextById

hashToObjectId :: DB m => H.Hash -> MaybeT m Db.ObjectId
hashToObjectId =
  m' "Q.loadHashId" Q.loadHashId . H.toBase32Hex
    >=> m' "Q.objectIdByPrimaryHashId" Q.objectIdByPrimaryHashId

loadObjectById :: DB m => Db.ObjectId -> MaybeT m ByteString
loadObjectById = m' "Q.loadObjectById" Q.loadObjectById

loadHashByObjectId :: DB m => Db.ObjectId -> MaybeT m H.Hash
loadHashByObjectId =
  fmap H.fromBase32Hex
    . m' "Q.loadPrimaryHashByObjectId" Q.loadPrimaryHashByObjectId

decodeComponentLengthOnly :: Applicative f => ByteString -> MaybeT f Word64
decodeComponentLengthOnly = m' "decodeComponentLengthOnly"
  (fmap pure $ getFromBytes S.lengthFramedArray)

decodeTermElement :: Applicative f => Word64 -> ByteString -> MaybeT f (LocalIds, S.Term.Term)
decodeTermElement i =
  m'
    ("getTermElement: " ++ show i ++ ") fromBytes:")
    (fmap pure $ getFromBytes $ S.lookupTermElement i)

decodeDeclElement :: Applicative f => Word64 -> ByteString -> MaybeT f (LocalIds, S.Decl.Decl Symbol)
decodeDeclElement i =
  m'
    ("getDeclElement: " ++ show i ++ ") fromBytes:")
    (pure . getFromBytes (S.lookupDeclElement i))

-- * legacy conversion helpers
getCycleLen :: DB m => H.Hash -> MaybeT m Word64
getCycleLen h = fmap fromIntegral $
  hashToObjectId >=> loadObjectById >=> decodeComponentLengthOnly $ h

getDeclTypeByReference :: DB m => C.Reference.Id -> MaybeT m C.Decl.DeclType
getDeclTypeByReference = fmap C.Decl.declType . loadDeclByReference

-- * meat and veggies

loadTermByReference :: DB m => C.Reference.Id -> MaybeT m (C.Term Symbol)
loadTermByReference (C.Reference.Id h i) = do
  -- retrieve and deserialize the blob
  (localIds, term) <-
    hashToObjectId >=> loadObjectById >=> decodeTermElement i $ h

  -- look up the text and hashes that are used by the term
  texts <- traverse loadTextById $ LocalIds.textLookup localIds
  hashes <- traverse loadHashByObjectId $ LocalIds.objectLookup localIds

  -- substitute the text and hashes back into the term
  let substText (S.Term.LocalTextId w) = texts Vector.! fromIntegral w
      substHash (S.Term.LocalDefnId w) = hashes Vector.! fromIntegral w
      substTermRef = bimap substText (fmap substHash)
      substTypeRef = bimap substText substHash
      substTermLink = bimap substTermRef substTypeRef
      substTypeLink = substTypeRef
  pure (C.Term.extraMap substText substTermRef substTypeRef substTermLink substTypeLink id term)

loadTypeOfTermByTermReference :: DB m => C.Reference.Id -> MaybeT m (C.Term.Type Symbol)
loadTypeOfTermByTermReference r = do
  -- convert query reference by looking up db ids
  r' <- C.Reference.idH hashToObjectId r
  -- load "type of term" blob for the reference
  bytes <- m' "Q.loadTypeOfTerm" Q.loadTypeOfTerm r'
  -- deserialize the blob into the type
  typ <- m' "getTypeFromBytes" (fmap pure $ getFromBytes $ S.getType S.getReference) bytes
  -- convert the result type by looking up db ids
  C.Type.rtraverse s2cReference typ

loadDeclByReference :: DB m => C.Reference.Id -> MaybeT m (C.Decl Symbol)
loadDeclByReference (C.Reference.Id h i) = do
  -- retrieve the blob
  (localIds, C.Decl.DataDeclaration dt m b ct) <- do
    hashToObjectId >=> loadObjectById >=> decodeDeclElement i $ h

  -- look up the text and hashes that are used by the term
  texts <- traverse loadTextById $ LocalIds.textLookup localIds
  hashes <- traverse loadHashByObjectId $ LocalIds.objectLookup localIds

  -- substitute the text and hashes back into the term
  let substText (S.Decl.LocalTextId w) = texts Vector.! fromIntegral w
      substHash (S.Decl.LocalTypeId w) = hashes Vector.! fromIntegral w
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

-- | returns a list of known definitions referencing `r`
dependents :: DB m => C.Reference -> MaybeT m (Set C.Reference.Id)
dependents r = do
  r' <- c2sReference r
  sIds :: [S.Reference.Id] <- Q.getDependentsForDependency r'
  -- how will you convert this back to Unison.Reference if you
  -- don't know the cycle size?
  cIds <- traverse s2cReferenceId sIds
  pure $ Set.fromList cIds

-- * Sync-related dependency queries

termDependencies :: DB m => C.Reference.Id -> m (Maybe (Set C.Reference.Id))
termDependencies = error "todo"

declDependencies :: DB m => C.Reference.Id -> m (Maybe (Set C.Reference.Id))
declDependencies = error "todo"

-- branchDependencies ::
--   Branch.Hash -> m (Maybe (CausalHash, BD.Dependencies)),
-- -- |the "new" terms and types mentioned in a patch
-- patchDependencies :: EditHash -> m (Set Reference, Set Reference)

-- getBranchByAnyHash ::
-- getBranchByBranchHash :: DB m => BranchHash -> m (Maybe (Branch m))
-- getBranchByCausalHash :: DB m => CausalHash -> m (Maybe (Branch m))

-- lca              :: (forall he e. [Causal m CausalHash he e] -> m (Maybe BranchHash)),
