{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}


{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Unison.Codebase2a.SyncV1V2 where

import Unison.Prelude hiding (Map)

import           Control.Monad.State.Strict     ( MonadState, evalStateT )
--import           Control.Monad.Writer.Strict    ( MonadWriter, execWriterT )
import Control.Monad.Reader (ask, runReaderT)
import Data.Bytes.Get (MonadGet)
import Data.Bytes.Put (MonadPut)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
--import qualified Data.Set as Set
--import           Data.Set (Set)
import Control.Lens (mapMOf, to)
import           Data.List.Extra (nubOrd)
import Data.Maybe (fromJust)
--import Safe (headMay)
import qualified Data.Text as Text
import Data.Void (Void)
--import qualified Control.Monad.Writer.Strict   as Writer
import qualified Unison.ABT as ABT
import qualified Unison.Codebase.Serialization as S
--import qualified Unison.Codebase                  as V1
import Unison.Codebase (CodebasePath)
import qualified Unison.Codebase.Serialization.V1 as S.V1
--import qualified Unison.Codebase.FileCodebase     as V1
import qualified Unison.Codebase.FileCodebase.Common as V1
import qualified Unison.Codebase.FileCodebase.Common as V1.FC
--import qualified Unison.Codebase.FileCodebase.Common as V1.Common
import qualified Unison.Codebase2a.Serialization.V2 as V2
import qualified Unison.Codebase2a.ObjectType as V2
--import qualified Unison.Codebase.FileCodebase.SlimCopyRegenerateIndex as V1Sync
--import qualified Unison.Codebase.Branch.Dependencies as V1
import qualified Unison.Codebase.Branch as V1.Branch
import qualified Unison.Codebase.Causal as V1
import qualified Unison.Codebase.Causal as V1.Causal
--import Unison.Codebase.SyncMode (SyncMode)
--import qualified Unison.Codebase.SyncMode as SyncMode
import Unison.Codebase2a.Base32Hex (Base32Hex)
import qualified Unison.Codebase2a.Base32Hex as Base32Hex
import qualified Unison.Codebase2a.Serialization.Db as Db
import Unison.Codebase2a.Serialization.Db (DB)
import Unison.Hash (Hash)
import qualified Unison.Hash as Hash
import qualified Unison.Hashable as H
import Data.String.Here.Uninterpolated (here)
import Unison.Reference (ReferenceH, Reference)
import qualified Unison.Reference as Reference
import Unison.Referent (ReferentH, Referent')
import qualified Unison.Referent as Referent
import Unison.NameSegment (NameSegment)
import Database.SQLite.Simple (Connection, Only(..))
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Control.Monad.Except (runExceptT, throwError, MonadError)
import UnliftIO.Directory (doesFileExist, listDirectory)
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import Unison.Util.Monoid (foldMapM)
import qualified Unison.LabeledDependency as LD
import Unison.Var (Var)
import Unison.Hashable (Hashable)
import Unison.Parser (Ann)
import Unison.Symbol (Symbol)
import Data.Coerce (coerce)
import qualified Unison.DataDeclaration as DD
import qualified Unison.Codebase.Patch as Patch
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.Codebase.TypeEdit as TypeEdit
import qualified Unison.Util.Relation as Relation

-- [ ] todo: need some way to associate types to terms.  let's reuse the type index
-- type_index does something similar?
-- type_mentions_index does something similar

data V1EntityRef
  = Decl1 Hash -- this will refer to the whole component
  | Term1 Hash -- "
  | Patch1 V1.Branch.EditHash
  | Branch1 V1.Branch.Hash
  deriving (Eq, Ord)

v1EntityRefToHash :: V1EntityRef -> Hash
v1EntityRefToHash = \case
  Decl1 h -> h
  Term1 h -> h
  Patch1 h -> h
  Branch1 (V1.Causal.RawHash h) -> h

newtype Base32HexPrefix = Base32HexPrefix Text
  deriving Show via Text
  deriving ToField via Text
  deriving FromField via Text

newtype PatchHash = PatchHash Hash
newtype NamespaceHash = NamespaceHash Hash
newtype CausalHash =  CausalHash Hash

data V2EntityRef = Decl2 Reference.Id
                | Term2 Reference.Id
                | Patch2 PatchHash
                | NamespaceHash2 NamespaceHash
                | CausalHash2 CausalHash

initializeV2DB :: MonadIO m => m ()
initializeV2DB = error "todo"

data FatalError = NoRootBranch
                | MissingBranch Hash
                | MissingPatch Hash
                | MissingTerm Reference.Id
                | MissingTermHash Hash
                | MissingTypeOfTerm Reference.Id
                | MissingDecl Reference.Id
                | MissingDeclHash Hash
                | InvalidBranch Hash
                | InvalidPatch Hash
                | InvalidTerm Reference.Id
                | InvalidTypeOfTerm Reference.Id
                | InvalidDecl Reference.Id

type Type = Type.Type Symbol Ann
type Term = Term.Term Symbol Ann
type Decl = DD.Decl Symbol Ann
type Patch = Patch.Patch
-- the S stands for "for serialization"
type Type2S = Type.TypeH (Maybe Db.ObjectId) Symbol Ann
type Term2S = Term.TermH (Maybe Db.ObjectId) Symbol Ann
type Decl2S = DD.DeclH (Maybe Db.ObjectId) Symbol Ann
type Patch2S = Patch.PatchH Db.ObjectId
--type Term2S = ABT.Term (Term.F' (Maybe TermId) DeclId (Type.TypeH DeclId Symbol ()) Void ()) Symbol ()
--alternative representation if embedded
--type Term2S = ABT.Term (Term.F' (Maybe TermId) DeclId TypeId Void ()) Symbol ()

fmtV@(S.Format getV putV) = S.V1.formatSymbol
fmtA@(S.Format getA putA) = V1.formatAnn


-- todo: this just converts a whole codebase, which we need to do locally \
--       but we also want some code that imports just a particular sub-branch.
syncV1V2 :: forall m. MonadIO m => Connection -> CodebasePath -> m (Either FatalError ())
syncV1V2 c rootDir = liftIO $ SQLite.withTransaction c . runExceptT . flip runReaderT c $ do
  v1RootHash <- getV1RootBranchHash rootDir >>= maybe (throwError NoRootBranch) pure
  -- starting from the root namespace, convert all entities you can find
  convertEntities [Branch1 v1RootHash]
  v2RootHash <- v2CausalHashForV1BranchHash v1RootHash
  setV2Root v2RootHash
  error "todo: compressEntities and vacuum db" v2RootHash

  -- Incorporating diff construction into the conversion is tough because
  -- a) I was thinking we'd represent an older version as a diff against the
  --    newer version, but the newer version hasn't been fully constructed
  --    until the older versions have been converted and hashed.
  -- b) If we just store all the old versions uncompressed, it might be too big.
  --    (But no bigger than the v1 db.) But if that is okay, we can compress and
  --    vacuum them afterwards.

  pure ()
  where
  setV2Root = error "todo: setV2Root"
  v2CausalHashForV1BranchHash = error "todo: v2CausalHashForV1BranchHash"
  convertEntities :: forall m. DB m
                  => MonadError FatalError m
                  => [V1EntityRef]
                  -> m ()
  convertEntities [] = pure ()
  convertEntities all@(h : rest) = do
    termDirComponents <- componentMapForDir (V1.termsDir rootDir)
    declsDirComponents <- componentMapForDir (V1.typesDir rootDir)
    case h of
      Branch1 (V1.unRawHash -> h) -> error "todo"
      Term1 h ->
        -- if this hash is already associated to an object
        ifM (existsObjectWithHash h) (convertEntities rest) $ do
          -- load a cycle from disk
          e <- loadTerm1 rootDir termDirComponents h
          matchTerm1Dependencies h e >>= \case
            Left missing -> convertEntities (missing ++ all)
            Right lookup -> do
              convertTerm1 lookup h e
              convertEntities rest
      Decl1 h ->
        ifM (existsObjectWithHash h) (convertEntities rest) $ do
          d <- loadDecl1 rootDir declsDirComponents h
          matchDecl1Dependencies h d >>= \case
            Left missing -> convertEntities (missing ++ all)
            Right lookup -> do
              convertDecl1 lookup h d
              convertEntities rest
      Patch1 h ->
        ifM (existsObjectWithHash h) (convertEntities rest) $ do
          p <- loadPatch1 rootDir h
          matchPatch1Dependencies h p >>= \case
            Left missing -> convertEntities (missing ++ all)
            Right lookup -> do
              hashId <- Db.saveHashByteString h
              savePatch hashId (Patch.hmap lookup p)
              convertEntities rest

  -- |load a causal branch raw thingo
  loadCausalBranch1 :: MonadIO m
                    => MonadError FatalError m
                    => Hash -> m (V1.Causal.Raw V1.Branch.Raw V1.Branch.Raw)
  loadCausalBranch1 h = do
    let file = V1.branchPath rootDir (V1.RawHash h)
    ifM (doesFileExist file)
      (S.getFromFile' (S.V1.getCausal0 S.V1.getRawBranch) file >>= \case
        Left err -> throwError $ InvalidBranch h
        Right c0  -> pure c0)
      (throwError $ MissingBranch h)

  matchCausalBranch1Dependencies = error "todo"
  convertCausalBranch1
    :: MonadIO m
    => MonadError FatalError m
    => (Hash -> V2EntityRef)
    -> V1.Causal.Raw V1.Branch.Raw V1.Branch.Raw
    -> m ()
  convertCausalBranch1 lookup causalBranch1 = do
    (rawBranch2 :: RawBranch) <- convertBranch1 lookup (V1.rawHead causalBranch1)
    rawCausal2 <- convertCausal1 lookup (H.hash @Hash rawBranch2) (V1.rawTails causalBranch1)
    saveBranch2 rawBranch2
    saveCausal2 rawCausal2
    where
    convertBranch1 lookup rawBranch1 = error "todo" lookup rawBranch1
    convertCausal1 lookup h2 tails = error "todo" lookup h2 tails
    saveBranch2 = error "todo"
    saveCausal2 = error "todo"

  -- | Given a V1 term component, convert and save it to the V2 codebase
  -- Pre-requisite: all hash-identified entities in the V1 component have
  -- already been converted and added to the V2 codebase, apart from self-
  -- references.
  convertTerm1 :: DB m' => (Hash -> Db.ObjectId) -> Hash -> [(Term, Type)] -> m' ()
  convertTerm1 lookup hash1 v1component = do
    -- the old terms include self-references.
    -- we want to convert those to object ids, but we can't because we won't
    -- know the id for self-references until the end.
    --
    -- hacky workaround: we convert self-references to `Nothing` and
    -- serialize those differently.
    let lookup' :: Hash -> Maybe Db.ObjectId
        lookup' h | h == hash1 = Nothing
        lookup' h = Just (lookup h)

    componentHashId <- Db.saveHashByteString hash1

    v2component :: [Term2S] <-
      for ([0..] `zip` v1component) $ \(i, (term1, typ1)) -> do
        let r = Db.Reference2Id componentHashId i
        let rt = Db.Referent2IdRef r

        -- What's up with that "Just"? Answer: it's a wart to do with using
        -- the same hash type for term and type references.  These types can't
        -- self-reference the term, so they're never properly Nothing.  I would
        -- prefer to enforce this through types though, and while I did experiment
        -- with that a bit, I got overwhelmed and backed off.  At the moment,
        -- I have a extra-richly parameterized Term.F' that supports different
        -- reference representations for calling terms (`termRepr`) vs ctors
        -- (`declRepr`).
        saveTypeOfReferent rt (Type.hmap (Just . lookup) typ1)

        createTypeIndicesForReferent rt typ1
        createDependencyIndexForTerm r term1

        pure (Term.hmap lookup' term1)

    -- if it were not the case that v2 terms use the same hash as v1 terms,
    -- we can associate the v1 and v2 hashes for the term separately.
    -- that is currently done in saveTermComponent
    saveTermComponent componentHashId v2component

  primaryHashByHash1 :: DB m => V2.ObjectType -> Hash -> m (Maybe Hash)
  primaryHashByHash1 t h =
    Db.query sql (t, Base32Hex.fromHash h) <&> \case
      [Only h] -> Just (Base32Hex.toHash h)
      [] -> Nothing
      hs -> error $ "hash2ForHash1 " ++ show t ++ " " ++
                  take 10 (show h) ++ " = " ++ (show . map (take 10 . show)) hs
    where
    sql = [here|
      SELECT v2hash.base32
      FROM hash AS v2hash
      INNER JOIN object ON object.primary_hash_id = v2hash.id
      INNER JOIN hash_object ON object.id = hash_object.object_id
      INNER JOIN hash AS v1hash ON hash_object.hash_id = v1hash.id
      WHERE object.type_id = ? AND v1hash.base32 = ?
    |]

  convertDecl1 :: DB m' => (Hash -> Db.ObjectId) -> Hash -> [Decl] -> m' ()
  convertDecl1 lookup hash1 v1component = do
    let lookup' :: Hash -> Maybe Db.ObjectId
        lookup' h | h == hash1 = Nothing
        lookup' h = Just (lookup h)

    componentHashId <- Db.saveHashByteString hash1

    v2component :: [Decl2S] <-
      for (zip v1component [0..]) $ \(decl1, i) -> do
        let r :: Db.Reference2Id = Db.Reference2Id componentHashId i

        for_ (DD.constructorTypes (DD.asDataDecl decl1) `zip` [0..]) $ \(type1, j) -> do
          let rt :: Db.Referent2Id = Db.Referent2IdCon r j

          saveTypeOfReferent rt (Type.hmap lookup' type1)
          createTypeIndicesForReferent rt type1

        createDependencyIndexForDecl r decl1

        -- produce a new Decl that references ObjectIds instead of Hashes
        pure (DD.hmapDecl lookup' decl1)

    saveDeclComponent componentHashId v2component

  loadBranch1 ::forall m. MonadIO m
              => MonadError FatalError m
              => m V1.Branch.Raw
  loadBranch1 = error "todo: loadBranch1"
  --      ifM (not <$> doesFileExist (V1.branchPath root h))
  --        (throwError $ MissingBranch h)
  --        (do
  --          branch1 <- loadBranch1
  --          )


newtype MdValuesH h = MdValues (Set (ReferenceH h))
deriving via (Set (ReferenceH h))
  instance Hashable (ReferenceH h) => Hashable (MdValuesH h)

type RawBranch = RawBranchH Hash Hash
data RawBranchH h h2 = RawBranch
  { terms :: Map NameSegment (Map (ReferentH h) (MdValuesH h))
  , types :: Map NameSegment (Map (ReferenceH h) (MdValuesH h))
  , patches :: Map NameSegment h
  , children :: Map NameSegment h2
  }

instance Hashable RawBranch where
  tokens b =
    [ H.accumulateToken (terms b)
    , H.accumulateToken (types b)
    , H.accumulateToken (patches b)
    , H.accumulateToken (children b)
    ]

getV1RootBranchHash :: MonadIO m => CodebasePath -> m (Maybe V1.Branch.Hash)
getV1RootBranchHash root = listDirectory (V1.branchHeadDir root) <&> \case
  [single] -> Just . V1.Branch.Hash . Hash.unsafeFromBase32Hex $ Text.pack single
  _ -> Nothing

-- |Look for an ObjectId corresponding to the provided V1 hash.
-- Returns Left if not found.
lookupObject :: DB m => V1EntityRef -> m (Either V1EntityRef (Hash, Db.ObjectId))
lookupObject r@(v1EntityRefToHash -> h) =
  getObjectIdByBase32Hex (Base32Hex.fromHash h) <&> \case
    Nothing -> Left r
    Just i -> Right (h, i)


--- sqlite utils
saveBase32Hash :: DB m => Base32Hex -> m Db.HashId
saveBase32Hash h = insert >> fmap fromJust (loadBase32Hash h) where
  insert = Db.execute sql (Only h) where
    sql = [here| INSERT OR IGNORE INTO hash (base32) VALUES (?) |]
loadBase32Hash :: DB m => Base32Hex -> m (Maybe Db.HashId)
loadBase32Hash h = Db.queryMaybe sql (Only h) where
  sql = [here| SELECT id FROM hash WHERE base32 = ? |]

saveTypeOfReferent :: DB m => Db.Referent2Id -> Type2S -> m ()
saveTypeOfReferent r type2s = let
  blob = S.putBytes (V2.putType putObjectId putV putA) type2s
  in Db.saveTypeOfReferent r blob

--saveTypeOfTermReference ::
--  DB m => (Hash -> Db.ObjectId) -> Db.Reference2Id -> Type -> m ()
--saveTypeOfTermReference lookup r typ = let
--  serializedType2 :: ByteString
--  serializedType2 = S.putBytes
--    (V2.putType putObjectId putV putA) (Type.hmap (Just . lookup) typ)
--  in Db.saveTypeOfReferent (Db.Referent2IdRef r) serializedType2
--
--saveTypeOfDeclReferent ::
--  DB m => (Hash -> Maybe Db.ObjectId) -> Db.Reference2Id -> Int -> Type -> m ()
--saveTypeOfDeclReferent lookup r cid typ = let
--  serializedType2 :: ByteString = S.putBytes
--    (V2.putType putObjectId putV putA) (Type.hmap lookup typ)
--  in Db.saveTypeOfReferent (Db.Referent2IdCon r cid) serializedType2

-- |Multiple hashes can map to a single object!
getObjectIdByBase32Hex :: DB m => Base32Hex -> m (Maybe Db.ObjectId)
getObjectIdByBase32Hex h = Db.queryMaybe sql (Only h)
  where
  sql = [here|
    SELECT object.id
    FROM hash
    INNER JOIN hash_object ON hash_object.hash_id = hash.id
    INNER JOIN object ON hash_object.object_id = object.id
    WHERE hash.base32 = ?
  |]

augmentLookup :: Ord a => (a -> b) -> Map a b -> a -> b
augmentLookup f m a = fromMaybe (f a) (Map.lookup a m)


-- * Serialization
putObjectId :: S.Put Db.ObjectId
putObjectId (Db.ObjectId i) = S.V1.putLength i
getObjectId :: S.Get Db.ObjectId
getObjectId = Db.ObjectId <$> S.V1.getLength

putUnit :: S.Put ()
putUnit () = pure ()

saveReferenceAsReference2 :: DB m => Reference -> m Db.Reference2
saveReferenceAsReference2 = mapMOf Db.referenceTraversal Db.saveHashByteString

-- |load a term component by its hash.
-- A v1 term component is split across an arbitrary number of files.
-- We have to 1) figure out what all the filenames are (preloaded into
-- `termDirComponents`), 2) load them all,
loadTerm1 :: MonadIO m
          => MonadError FatalError m
          => CodebasePath
          -> Map Hash [Reference.Id]
          -> Hash
          -> m [(Term, Type)]
loadTerm1 rootDir componentsFromDir h = case Map.lookup h componentsFromDir of
  Nothing -> throwError $ MissingTermHash h
  Just set -> case toList set of
    [] -> error "Just [] shouldn't occur here."
    Reference.Id h _i n : _etc -> for [0..n-1] \i -> do
      let r = Reference.Id h i n
      term <- V1.FC.getTerm (S.get fmtV) (S.get fmtA) rootDir r
        >>= maybe (throwError $ MissingTerm r) pure
      typeOfTerm <- V1.FC.getTypeOfTerm (S.get fmtV) (S.get fmtA) rootDir r
        >>= maybe (throwError $ MissingTypeOfTerm r) pure
      pure (term, typeOfTerm)

loadDecl1 :: MonadIO m => MonadError FatalError m
  => CodebasePath -> Map Hash [Reference.Id] -> Hash -> m [Decl]
loadDecl1 rootDir componentsFromDir h = case Map.lookup h componentsFromDir of
  Nothing -> throwError $ MissingDeclHash h
  Just set -> case toList set of
    [] -> error "Just [] shouldn't occur here."
    Reference.Id h _i n : _etc -> for [0..n-1] \i -> do
      let r = Reference.Id h i n
      V1.FC.getDecl (S.get fmtV) (S.get fmtA) rootDir r
        >>= maybe (throwError $ MissingDecl r) pure

-- |load a patch
loadPatch1 rootDir h = do
  let file = V1.editsPath rootDir h
  ifM (doesFileExist file)
    (S.getFromFile' S.V1.getEdits file >>= \case
      Left  _err   -> throwError (InvalidPatch h)
      Right edits -> pure edits)
    (throwError $ MissingPatch h)

-- 3) figure out what their combined dependencies are
matchTerm1Dependencies ::
  DB m => Hash -> [(Term, Type)] -> m (Either [V1EntityRef] (Hash -> Db.ObjectId))
matchTerm1Dependencies componentHash tms = let
  -- Get a list of Eithers corresponding to the non-self dependencies of this term.
  lookupDependencyObjects
    :: DB m => (Term, Type) -> m [Either V1EntityRef (Hash, Db.ObjectId)]
  lookupDependencyObjects (term, typeOfTerm) = traverse lookupObject deps
    where
    (termTypeDeps, termTermDeps) =
      partitionEithers
        . map LD.toReference
        . toList
        $ Term.labeledDependencies term
    deps = nubOrd $
      [ Decl1 h | Reference.Derived h _i _n <- toList $ Type.dependencies typeOfTerm ] <>
      [ Decl1 h | Reference.Derived h _i _n <- termTypeDeps ] <>
      [ Term1 h | Reference.Derived h _i _n <- termTermDeps
                , h /= componentHash ] -- don't include self-refs 😬
  in do
    -- check the lefts, if empty then everything is on the right;
    -- else return left.
    (missing, found) <- partitionEithers <$> foldMapM lookupDependencyObjects tms
    pure $ case missing of
      [] -> Right (makeLookup "term component" componentHash found)
      missing -> Left missing

matchDecl1Dependencies ::
  DB m => Hash -> [Decl] -> m (Either [V1EntityRef] (Hash -> Db.ObjectId))
matchDecl1Dependencies componentHash decls = let
  lookupDependencyObjects
    :: DB m => Decl -> m [Either V1EntityRef (Hash, Db.ObjectId)]
  lookupDependencyObjects decl = traverse lookupObject . nubOrd $
    [ Decl1 h | Reference.Derived h _i _n <- toList (DD.declDependencies decl)
              , h /= componentHash ]
  in do
    (missing, found) <- partitionEithers <$> foldMapM lookupDependencyObjects decls
    pure $ case missing of
      [] -> Right (makeLookup "decl component" componentHash found)
      missing -> Left missing

matchPatch1Dependencies h (Patch.Patch tms tps) = do
  deps :: [Either V1EntityRef (Hash, Db.ObjectId)] <-
    traverse lookupObject . nubOrd $
      [ Term1 h | (r, e) <- Relation.toList tms
      , Reference.Derived h _i _n <- r : TermEdit.references e ] ++
      [ Decl1 h | (r, e) <- Relation.toList tps
      , Reference.Derived h _i _n <- r : TypeEdit.references e]
  let (missing, found) = partitionEithers deps
  pure $ case missing of
    [] -> Right (makeLookup "patch" h found)
    missing -> Left missing

makeLookup :: String -> Hash -> [(Hash, Db.ObjectId)] -> Hash -> Db.ObjectId
makeLookup lookupDescription h l a = case Map.lookup a m of
  Just b -> b
  Nothing ->
    error $ "Somehow I don't have the ObjectId for "
      ++ show (Base32Hex.fromHash a)
      ++ " in the map for "
      ++ lookupDescription ++ " "
      ++ show (Base32Hex.fromHash h)
  where m = Map.fromList l

--
createTypeIndicesForReferent :: DB m => Db.Referent2Id -> Type -> m ()
createTypeIndicesForReferent r typ = do
  let typeForIndexing = Type.removeAllEffectVars typ

  -- add the term to the type index
  typeReferenceForIndexing :: Db.Reference2 <-
    saveReferenceAsReference2 (Type.toReference typeForIndexing)

  Db.addToFindByTypeIndex r typeReferenceForIndexing

  -- add the term to the type mentions index
  typeMentionsForIndexing :: [Db.Reference2] <-
    traverse
      saveReferenceAsReference2
      (toList $ Type.toReferenceMentions typeForIndexing)

  traverse_ (Db.addToFindByTypeMentionsIndex r) typeMentionsForIndexing

  where
  addTermToFindByTypeIndex :: DB m => Db.Referent2Id -> Reference -> m ()
  addTermToFindByTypeIndex termRef typeRef = do
    typeRef2 :: Db.Reference2 <-
      saveReferenceAsReference2 typeRef
    Db.addToFindByTypeIndex termRef typeRef2

  addTermToTypeMentionsIndex ::
    (DB m, Foldable f) => Db.Referent2Id -> f Reference -> m ()
  addTermToTypeMentionsIndex termRef typeRefs = do
    typeRefs2 :: [Db.Reference2] <-
      traverse saveReferenceAsReference2 (toList typeRefs)
    traverse_ (Db.addToFindByTypeMentionsIndex termRef) typeRefs2

createDependencyIndexForTerm :: DB m => Db.Reference2Id -> Term -> m ()
createDependencyIndexForTerm dependentId dependent =
  let
    -- Note: This includes self references, so if a use case needs such
    -- references to be filtered out, then they need to be filtered out.
    dependencies :: Set Reference
    dependencies = Term.dependencies dependent
  in do
    dependencies2 :: [Db.Reference2] <-
      traverse saveReferenceAsReference2 (toList dependencies)
    traverse_ (Db.addDependencyToIndex dependentId) dependencies2

createDependencyIndexForDecl :: DB m => Db.Reference2Id -> Decl -> m ()
createDependencyIndexForDecl dependentId dependent =
  let
    -- Note: This includes self references, so if a use case needs such
    -- references to be filtered out, then they need to be filtered out.
    dependencies :: Set Reference
    dependencies = DD.declDependencies dependent
  in do
    dependencies2 :: [Db.Reference2] <-
      traverse saveReferenceAsReference2 (toList dependencies)
    traverse_ (Db.addDependencyToIndex dependentId) dependencies2

saveTermComponent :: DB m => Db.HashId -> [Term2S] -> m ()
saveTermComponent h component = do
  o <- Db.saveObject h V2.TermCycle blob
  Db.saveHashObject h o 2
  where
  blob = S.putBytes (S.V1.putFoldable (V2.putTerm putObjectId putV putA)) component

saveDeclComponent :: DB m => Db.HashId -> [Decl2S] -> m ()
saveDeclComponent h component = do
  o <- Db.saveObject h V2.DeclCycle blob
  Db.saveHashObject h o 2
  where
  blob = S.putBytes (S.V1.putFoldable (V2.putDecl putObjectId putV putA)) component

savePatch :: DB m => Db.HashId -> Patch2S -> m ()
savePatch h p = do
  o <- Db.saveObject h V2.Patch (S.putBytes (S.V1.putEditsH putObjectId) p)
  Db.saveHashObject h o 2

-- |Loads a dir with format <root>/base32-encoded-reference.id...
-- into a map from Hash to component references
componentMapForDir :: forall m. MonadIO m => FilePath -> m (Map Hash [Reference.Id])
componentMapForDir root = listDirectory root <&> foldl' insert mempty
  where
  insert m filename = case V1.componentIdFromString filename of
    Nothing -> m -- skip silently
    Just r@(Reference.Id h _i _n) ->
      Map.unionWith (<>) m (Map.singleton h [r])

existsObjectWithHash :: DB m => Hash -> m Bool
existsObjectWithHash h = Db.queryExists sql [Base32Hex.fromHash h] where
  sql = [here|
    SELECT 1
    FROM hash INNER JOIN hash_object ON hash.id = hash_object.hash_id
    WHERE base32 = ?
  |]


----- graveyard
---- |True if `h` (just the hash!) is interned in the DB
--knownHash :: DB m => Hash -> m Bool
--knownHash h = anyExists $ Db.query sql [Base32Hex.fromHash h] where
--  sql = [here| SELECT 1 FROM hash WHERE base32 = ? |]

--saveReference :: DB m => ReferenceH h -> m Db.ReferenceId
--saveReference r = insert r >> fmap fromJust (loadReference r) where
--  insert = \case
--    Reference.Builtin t -> execute sql (Just t, Nothing)
--    Reference.DerivedId idH -> do
--      rdId <- saveReferenceDerived idH
--      Db.execute sql (Nothing, Just rdId)
--  sql = [here|
--    INSERT OR IGNORE INTO reference (builtin, reference_derived_id)
--    VALUES (?, ?)
--  |]

--loadReferenceByHashId :: DB m => ReferenceH HashId -> m (Maybe ReferenceId)
--loadReferenceByHashId = \case
--  Reference.Builtin t -> queryMaybe sqlb (Only t)
--  Reference.DerivedId idH ->
--    loadReferenceDerivedByHashId idH >>= \case
--      Nothing -> pure Nothing
--      Just rdId -> queryMaybe sqld (Only rdId)
--  where
--  sqlb = [here| SELECT id FROM reference WHERE builtin = ? |]
--  sqld = [here| SELECT id FROM reference WHERE reference_derived_id = ? |]


--saveReferenceDerived :: DB m => Reference.Id -> m Db.ReferenceDerivedId
--saveReferenceDerived r@(Reference.Id h i _n) = do
--  hashId <- saveHashByteString h
--  saveReferenceDerivedByHashId (Reference.IdH hashId i _n)
--
--saveReferenceDerivedByHashId :: DB m => Reference.IdH Db.HashId -> m Db.ReferenceDerivedId
--saveReferenceDerivedByHashId r@(Reference.IdH hashId i _n) =
--  insert hashId i >> fmap fromJust (loadReferenceDerivedByHashId r) where
--  insert h i = liftIO $ execute sql (h, i) where
--    sql = [here|
--      INSERT OR IGNORE INTO reference_derived (hash_id, component_index)
--      VALUES (?, ?)
--    |]
--
--loadReferenceDerivedByHashId :: DB m => Reference.IdH Db.HashId -> m (Maybe Db.ReferenceDerivedId)
--loadReferenceDerivedByHashId (Reference.IdH h i _n) =
--  queryMaybe sql (h, i) where
--  sql = [here|
--    SELECT id FROM reference_derived
--    WHERE hash_id = ? AND component_index = ?
--  |]

--saveReferentDerived :: DB m => Referent.Id -> m ReferentDerivedId
--saveReferentDerived = error "todo"
--loadReferentDerived :: DB m => Referent.Id -> m (Maybe ReferentDerivedId)
--loadReferentDerived = error "todo"
--
--saveReferentDerivedByReferenceDerivedId :: DB m => Referent' ReferenceDerivedId -> m ReferentDerivedId
--saveReferentDerivedByReferenceDerivedId r = do
--  liftIO $ execute sql r
--  fmap fromJust (loadReferenceDerivedByReferenceDerivedId r)
--  where
--  sql = [here|
--    INSERT OR IGNORE INTO referent_derived
--      (reference_derived_id, constructor_id, constructor_type)
--    VALUES (?, ?, ?)
--  |]
--loadReferentDerivedByReferenceDerivedId :: DB m => Referent' ReferenceDerivedId -> m (Maybe ReferentDerivedId)
--loadReferentDerivedByReferenceDerivedId r =  queryMaybe . query sql r where
--  sql = [here|
--    SELECT id FROM referent_derived
--    WHERE reference_derived_id = ?
--      AND constructor_id = ?
--      AND constructor_type = ?
--  |]
