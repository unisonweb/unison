{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module U.Codebase.Sqlite.Operations where

import Control.Monad (join)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Bitraversable (Bitraversable (bitraverse))
import Data.ByteString (ByteString)
import Data.Bytes.Get (runGetS)
import Data.Functor ((<&>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as Vector
import Data.Word (Word64)
import U.Codebase.Decl (ConstructorId)
import qualified U.Codebase.Decl as C
import qualified U.Codebase.Decl as C.Decl
import qualified U.Codebase.Reference as C
import qualified U.Codebase.Reference as C.Reference
import qualified U.Codebase.Referent as C.Referent
import U.Codebase.ShortHash (ShortBranchHash)
import qualified U.Codebase.Sqlite.DbId as Db
import qualified U.Codebase.Sqlite.Decl.Format as S.Decl
import U.Codebase.Sqlite.LocalIds (LocalIds)
import qualified U.Codebase.Sqlite.LocalIds as LocalIds
import qualified U.Codebase.Sqlite.ObjectType as OT
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
import qualified U.Util.Monoid as Monoid
import U.Util.Serialization (Get, getFromBytes)
import qualified U.Util.Serialization as S

type Err m = MonadError Error m

type EDB m = (Err m, DB m)

type ErrString = String

data DecodeError
  = ErrTermElement Word64
  | ErrDeclElement Word64
  | ErrFramedArrayLen
  deriving (Show)

data Error
  = DecodeError DecodeError ByteString ErrString
  | DatabaseIntegrityError Q.Integrity
  | LegacyUnknownCycleLen H.Hash
  | LegacyUnknownConstructorType H.Hash C.Reference.Pos
  deriving (Show)

getFromBytesOr :: Err m => DecodeError -> Get a -> ByteString -> m a
getFromBytesOr e get bs = case runGetS get bs of
  Left err -> throwError (DecodeError e bs err)
  Right a -> pure a

liftQ :: Err m => ExceptT Q.Integrity m a -> m a
liftQ a =
  runExceptT a >>= \case
    Left e -> throwError (DatabaseIntegrityError e)
    Right a -> pure a

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

c2sReference :: EDB m => C.Reference -> MaybeT m S.Reference
c2sReference = bitraverse lookupTextId hashToObjectId

s2cReference :: EDB m => S.Reference -> m C.Reference
s2cReference = bitraverse loadTextById loadHashByObjectId

c2sReferenceId :: EDB m => C.Reference.Id -> MaybeT m S.Reference.Id
c2sReferenceId = C.Reference.idH hashToObjectId

s2cReferenceId :: EDB m => S.Reference.Id -> m C.Reference.Id
s2cReferenceId = C.Reference.idH loadHashByObjectId

lookupTextId :: DB m => Text -> MaybeT m Db.TextId
lookupTextId = m Q.loadText

loadTextById :: EDB m => Db.TextId -> m Text
loadTextById = liftQ . Q.loadTextById

-- ok to fail
hashToObjectId :: EDB m => H.Hash -> MaybeT m Db.ObjectId
hashToObjectId h = do
  hashId <- MaybeT $ Q.loadHashId . H.toBase32Hex $ h
  liftQ $ Q.objectIdByPrimaryHashId hashId

loadHashByObjectId :: EDB m => Db.ObjectId -> m H.Hash
loadHashByObjectId = fmap H.fromBase32Hex . liftQ . Q.loadPrimaryHashByObjectId

decodeComponentLengthOnly :: Err m => ByteString -> m Word64
decodeComponentLengthOnly = getFromBytesOr ErrFramedArrayLen S.lengthFramedArray

decodeTermElement :: Err m => C.Reference.Pos -> ByteString -> m (LocalIds, S.Term.Term)
decodeTermElement i = getFromBytesOr (ErrTermElement i) (S.lookupTermElement i)

decodeDeclElement :: Err m => Word64 -> ByteString -> m (LocalIds, S.Decl.Decl Symbol)
decodeDeclElement i = getFromBytesOr (ErrDeclElement i) (S.lookupDeclElement i)

-- * legacy conversion helpers

getCycleLen :: EDB m => H.Hash -> m Word64
getCycleLen h = do
  runMaybeT (hashToObjectId h)
    >>= maybe (throwError $ LegacyUnknownCycleLen h) pure
    >>= liftQ . Q.loadObjectById
    >>= decodeComponentLengthOnly
    >>= pure . fromIntegral

getDeclTypeByReference :: EDB m => C.Reference.Id -> m C.Decl.DeclType
getDeclTypeByReference r@(C.Reference.Id h pos) =
  runMaybeT (loadDeclByReference r)
    >>= maybe (throwError $ LegacyUnknownConstructorType h pos) pure
    >>= pure . C.Decl.declType

-- * meat and veggies

loadTermByReference :: EDB m => C.Reference.Id -> MaybeT m (C.Term Symbol)
loadTermByReference (C.Reference.Id h i) = do
  -- retrieve and deserialize the blob
  (localIds, term) <-
    hashToObjectId h >>= liftQ . Q.loadObjectById >>= decodeTermElement i

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

loadTypeOfTermByTermReference :: EDB m => C.Reference.Id -> MaybeT m (C.Term.Type Symbol)
loadTypeOfTermByTermReference r = do
  -- convert query reference by looking up db ids
  r' <- C.Reference.idH hashToObjectId r
  -- load "type of term" blob for the reference
  bytes <- m' "Q.loadTypeOfTerm" Q.loadTypeOfTerm r'
  -- deserialize the blob into the type
  typ <- m' "getTypeFromBytes" (fmap pure $ getFromBytes $ S.getType S.getReference) bytes
  -- convert the result type by looking up db ids
  C.Type.rtraverse s2cReference typ

loadDeclByReference :: EDB m => C.Reference.Id -> MaybeT m (C.Decl Symbol)
loadDeclByReference (C.Reference.Id h i) = do
  -- retrieve the blob
  (localIds, C.Decl.DataDeclaration dt m b ct) <-
    hashToObjectId h >>= liftQ . Q.loadObjectById >>= decodeDeclElement i

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

-- something kind of funny here.  first, we don't need to enumerate all the reference pos if we're just picking one
-- second, it would be nice if we could leave these as S.References a little longer
-- so that we remember how to blow up if they're missing
componentReferencesByPrefix :: EDB m => OT.ObjectType -> Text -> Maybe C.Reference.Pos -> m [S.Reference.Id]
componentReferencesByPrefix ot b32prefix pos = do
  oIds :: [Db.ObjectId] <- Q.objectIdByBase32Prefix ot b32prefix
  let test = maybe (const True) (==) pos
  let filterComponent l = [x | x@(C.Reference.Id _ pos) <- l, test pos]
  fmap Monoid.fromMaybe . runMaybeT $
    join <$> traverse (fmap filterComponent . componentByObjectIdS) oIds

termReferencesByPrefix :: EDB m => Text -> Maybe Word64 -> m [C.Reference.Id]
termReferencesByPrefix t w =
  componentReferencesByPrefix OT.TermComponent t w
    >>= traverse (C.Reference.idH loadHashByObjectId)

declReferencesByPrefix :: EDB m => Text -> Maybe Word64 -> m [C.Reference.Id]
declReferencesByPrefix t w =
  componentReferencesByPrefix OT.DeclComponent t w
    >>= traverse (C.Reference.idH loadHashByObjectId)

termReferentsByPrefix :: EDB m => Text -> Maybe Word64 -> m [C.Referent.Id]
termReferentsByPrefix b32prefix pos =
  fmap C.Referent.RefId <$> termReferencesByPrefix b32prefix pos

-- todo: simplify this if we stop caring about constructor type
-- todo: remove the cycle length once we drop it from Unison.Reference
declReferentsByPrefix ::
  EDB m =>
  Text ->
  Maybe C.Reference.Pos ->
  Maybe ConstructorId ->
  m [(H.Hash, C.Reference.Pos, Word64, C.DeclType, [C.Decl.ConstructorId])]
declReferentsByPrefix b32prefix pos cid = do
  componentReferencesByPrefix OT.DeclComponent b32prefix pos
    >>= traverse (loadConstructors cid)
  where
    loadConstructors :: EDB m => Maybe Word64 -> S.Reference.Id -> m (H.Hash, C.Reference.Pos, Word64, C.DeclType, [ConstructorId])
    loadConstructors cid rid@(C.Reference.Id oId pos) = do
      (dt, len, ctorCount) <- getDeclCtorCount rid
      h <- loadHashByObjectId oId
      let test :: ConstructorId -> Bool
          test = maybe (const True) (==) cid
          cids = [cid | cid <- [0 :: ConstructorId .. ctorCount - 1], test cid]
      pure (h, pos, len, dt, cids)
    getDeclCtorCount :: EDB m => S.Reference.Id -> m (C.Decl.DeclType, Word64, ConstructorId)
    getDeclCtorCount (C.Reference.Id r i) = do
      bs <- liftQ (Q.loadObjectById r)
      len <- decodeComponentLengthOnly bs
      (_localIds, decl) <- decodeDeclElement i bs
      pure (C.Decl.declType decl, len, fromIntegral $ length (C.Decl.constructorTypes decl))

-- (localIds, C.Decl.DataDeclaration dt m b ct) <-
--   hashToObjectId h >>= liftQ . Q.loadObjectById >>= decodeDeclElement i

-- consider getting rid of this function, or making it produce [S.Reference.Id]
componentByObjectId :: EDB m => Db.ObjectId -> m [C.Reference.Id]
componentByObjectId id = do
  len <- (liftQ . Q.loadObjectById) id >>= decodeComponentLengthOnly
  hash <- loadHashByObjectId id
  pure [C.Reference.Id hash i | i <- [0 .. len - 1]]

componentByObjectIdS :: EDB m => Db.ObjectId -> m [S.Reference.Id]
componentByObjectIdS id = do
  len <- (liftQ . Q.loadObjectById) id >>= decodeComponentLengthOnly
  pure [C.Reference.Id id i | i <- [0 .. len - 1]]

-- termReferentsByPrefix :: DB m => ShortHash -> m (Set C.Referent.Id)
-- termReferentsByPrefix = error "todo"

branchHashesByPrefix :: DB m => ShortBranchHash -> m (Set C.Reference.Id)
branchHashesByPrefix = error "todo"

-- | returns a list of known definitions referencing `r`
dependents :: EDB m => C.Reference -> MaybeT m (Set C.Reference.Id)
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
