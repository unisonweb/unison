{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
-- {-# OPTIONS_GHC -Wno-unused-local-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module U.Codebase.Convert.SyncV1V2 where

import Control.Lens (mapMOf, over)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.Extra ((>=>), ifM)
import Control.Monad.Reader (ReaderT (runReaderT))
import qualified Control.Monad.State as State
import Control.Monad.State (State)
import Data.Bifunctor (Bifunctor (first), second)
import Data.Bytes.Get (MonadGet)
import Data.Either (partitionEithers)
import Data.Either.Extra (mapLeft)
import Data.Foldable (Foldable (foldl', toList), for_, traverse_)
import Data.Functor ((<&>))
import qualified Data.List as List
import Data.List.Extra (nubOrd)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Traversable (for)
import Data.Tuple (swap)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField (ToField)
import qualified U.Codebase.Convert.TermUtil as TermUtil
import qualified U.Codebase.Convert.TypeUtil as TypeUtil
import qualified U.Codebase.Decl as V2.Decl
import qualified U.Codebase.Kind as V2.Kind
import qualified U.Codebase.Reference as V2.Reference
import qualified U.Codebase.Referent as V2.Referent
import Data.String.Here.Uninterpolated (here)
import qualified U.Codebase.Sqlite.DbId as Db
import qualified U.Codebase.Sqlite.Queries as Db
import qualified U.Codebase.Sqlite.Reference as V2S.Reference
import qualified U.Codebase.Sqlite.Reference as V2.Sqlite.Reference
import qualified U.Codebase.Sqlite.Referent as V2.Sqlite.Referent
import qualified U.Codebase.Sqlite.Serialization as S.V2
import qualified U.Codebase.Sqlite.Symbol as V2.Symbol
import qualified U.Codebase.Sqlite.Term.Format as V2.TermFormat
import qualified U.Codebase.Term as V2.Term
import qualified U.Codebase.Sqlite.Decl.Format as V2.DeclFormat
import qualified U.Codebase.Sqlite.LocalIds as V2.LocalIds
import qualified U.Codebase.Sqlite.ObjectType as V2.OT
import U.Codebase.Sqlite.Queries (DB)
import qualified U.Codebase.Type as V2.Type
import qualified U.Core.ABT as V2.ABT
import qualified U.Util.Base32Hex as Base32Hex
import U.Util.Base32Hex (Base32Hex)
import U.Util.Hash (Hash)
import qualified U.Util.Hash as Hash
import qualified U.Util.Hashable as H
import U.Util.Monoid (foldMapM)
import qualified U.Util.Serialization as S
import qualified Unison.Codebase.V1.ABT as V1.ABT
import qualified Unison.Codebase.V1.Branch.Raw as V1
import qualified Unison.Codebase.V1.DataDeclaration as V1.DD
import qualified Unison.Codebase.V1.FileCodebase as V1
import Unison.Codebase.V1.FileCodebase (CodebasePath)
import qualified Unison.Codebase.V1.FileCodebase as V1.FC
import qualified Unison.Codebase.V1.LabeledDependency as V1.LD
import qualified Unison.Codebase.V1.Reference as V1.Reference
import qualified Unison.Codebase.V1.Referent as V1.Referent
import qualified Unison.Codebase.V1.Serialization.Serialization as V1.S
import qualified Unison.Codebase.V1.Serialization.V1 as V1.S
import qualified Unison.Codebase.V1.Symbol as V1.Symbol
import qualified Unison.Codebase.V1.Term as V1.Term
import qualified Unison.Codebase.V1.Term.Pattern as V1.Pattern
import qualified Unison.Codebase.V1.Type as V1.Type
import qualified Unison.Codebase.V1.Type.Kind as V1.Kind
import UnliftIO (MonadIO, liftIO)
import UnliftIO.Directory (listDirectory)
import Data.Set (Set)
import Data.Bifunctor (Bifunctor(bimap))

newtype V1 a = V1 {runV1 :: a} deriving (Eq, Ord, Show)

newtype V2 a = V2 {runV2 :: a}
  deriving (Eq, Ord, Show, Functor)
  deriving (FromField, H.Accumulate, H.Hashable) via a

data V1EntityRef
  = Decl1 (V1 Hash) -- this will refer to the whole component
  | Term1 (V1 Hash) -- ditto
  | Patch1 V1.EditHash
  | Branch1 V1.BranchHash
  deriving (Eq, Ord, Show)

v1EntityRefToHash :: V1EntityRef -> V1 Hash
v1EntityRefToHash = \case
  Decl1 h -> h
  Term1 h -> h
  Patch1 (V1.EditHash h) -> V1 h
  Branch1 (V1.BranchHash h) -> V1 h

newtype Base32HexPrefix = Base32HexPrefix Text
  deriving (Show) via Text
  deriving (ToField) via Text
  deriving (FromField) via Text

-- newtype PatchHash h = PatchHash h
-- newtype NamespaceHash h = NamespaceHash h
newtype CausalHash h = CausalHash h

-- 1. Load a V1 component (Hash, [V1Term])
-- 2. Convert its dependencies before continuing
-- 3. Construct the hashable data structure
-- 4. Serialize the hashable data structure

-- unhashTermComponent :: V1 Hash -> [V1Term.Term] -> [V2.Term Symbol]

-- -- |things that appear in a deserialized RawBranch
-- type V2EntityRef =
--   V2EntityRefH
--     Hash
--     (PatchHash Hash)
--     (NamespaceHash Hash)
--     (CausalHash Hash)

-- | things that appear in a serialized RawBranch
--  type V2EntityRefS =
--      V2EntityRef
--        Db.ObjectId
--        (PatchHash Db.ObjectId)
--        (NamespaceHash Db.NamespaceHashId)
--        (CausalHash Db.CausalHashId)

-- data V2EntityRef hr hp hn hc
--   = Decl2 V2.Reference.Id
--   | Term2 V2.Reference.Id
--   | Patch2 PatchHash
--   | NamespaceHash2 V2NamespaceHash
--   | CausalHash2 CausalHash

-- initializeV2DB :: MonadIO m => m ()
-- initializeV2DB = error "todo"

data FatalError
  = NoRootBranch
  | MissingBranch (V1 Hash)
  | MissingPatch (V1 Hash)
  | MissingTerm V1.Reference.Id
  | MissingTermHash (V1 Hash)
  | MissingTypeOfTerm V1.Reference.Id
  | MissingDecl V1.Reference.Id
  | MissingDeclHash (V1 Hash)
  | InvalidBranch (V1 Hash)
  | InvalidPatch (V1 Hash)
  | InvalidTerm V1.Reference.Id
  | InvalidTypeOfTerm V1.Reference.Id
  | InvalidDecl V1.Reference.Id


type V1Type = V1.Type.Type V1.Symbol.Symbol ()
type V1Term = V1.Term.Term V1.Symbol.Symbol ()

type V1Decl = V1.DD.Decl V1.Symbol.Symbol ()

type V2HashTerm = V2.Term.Term V2.Symbol.Symbol
type V2HashTypeOfTerm = V2.Type.TypeT V2.Symbol.Symbol

type V2DiskTypeOfTerm = V2.Type.TypeR V2.Sqlite.Reference.Reference V2.Symbol.Symbol
type V2HashTermComponent = [V2HashTerm]

type V2DiskTermComponent = V2.TermFormat.LocallyIndexedComponent


type V2HashDecl = V2.Decl.Decl V2.Symbol.Symbol
type V2TypeOfConstructor = V2.Type.TypeD V2.Symbol.Symbol

type V2HashDeclComponent = [V2HashDecl]
type V2DiskDeclComponent = V2.DeclFormat.LocallyIndexedComponent

-- type Patch = Patch.Patch V1.Reference

-- -- the H stands for "for hashing"
-- -- the S stands for "for serialization"

-- type Term2ComponentH = [Term2 Hash]
-- type Term2ComponentS = [Term2 Db.ObjectId]

-- type Decl2ComponentH = [Decl2 (Maybe Hash)]
-- type Decl2S = Decl2 Db.ObjectId
-- type Decl2ComponentS = [Decl2S]

-- -- these have maybes in them to indicate a self-component reference
-- type Term2 h = V2.Term h
-- type Decl2 h = DD.DeclR (V2.Reference h) Symbol ()

-- -- for indexing
-- type Decl2I = DD.DeclR (V2.Reference Db.ObjectId) Symbol ()
-- type Term2S = Term2 Db.ObjectId
-- type Type2S = V2.Type Db.ObjectId

-- -- what about referent types in the index?
-- -- type CtorType2S = Type.TypeH Db.ObjectId Symbol Ann
-- -- type Term2S = Term.TermH (Maybe Db.ObjectId) Symbol Ann
-- type Patch2S = Patch.Patch (V2.Reference Db.ObjectId)

-- --type Term2S = ABT.Term (Term.F' (Maybe TermId) DeclId (Type.TypeH DeclId Symbol ()) Void ()) Symbol ()
-- --alternative representation if embedded
-- --type Term2S = ABT.Term (Term.F' (Maybe TermId) DeclId TypeId Void ()) Symbol ()

-- fmtV :: S.Format Symbol
-- fmtV = S.V1.formatSymbol

-- getV :: S.Get Symbol
-- getV = S.get S.V1.formatSymbol

-- putV :: S.Put Symbol
-- putV = S.put fmtV

-- fmtA :: S.Format Ann
-- fmtA = V1.formatAnn

-- getA :: S.Get Ann
-- getA = S.get fmtA

-- putA :: S.Put Ann
-- putA = S.put fmtA

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
    convertEntities ::
      forall m.
      DB m =>
      MonadError FatalError m =>
      [V1EntityRef] ->
      m ()
    convertEntities [] = pure ()
    convertEntities all@(h : rest) = do
      termDirComponents <- componentMapForDir (V1.termsDir rootDir)
      declsDirComponents <- componentMapForDir (V1.typesDir rootDir)
      case h of
        Term1 h ->
          -- if this hash is already associated to an object
          ifM (existsObjectWithHash (runV1 h)) (convertEntities rest) $ do
            -- load a cycle from disk
            e <- loadTerm1 rootDir termDirComponents h
            matchTerm1Dependencies h e >>= \case
              Left missing -> convertEntities (missing ++ all)
              Right (getHash, getObjId, getTextId) -> do
                convertTerm1 getHash getObjId getTextId h e
                convertEntities rest
-- Decl1 h ->
--   ifM (existsObjectWithHash (runV1 h)) (convertEntities rest) $ do
--     d <- loadDecl1 rootDir declsDirComponents h
--     matchDecl1Dependencies h d >>= \case
--       Left missing -> convertEntities (missing ++ all)
--       Right lookup -> do
--         convertDecl1 (error "todo: lookup") h d
--         convertEntities rest
-- Patch1 h ->
--   ifM (existsObjectWithHash (runV1 h)) (convertEntities rest) $ do
--     p <- loadPatch1 rootDir h
--     matchPatch1Dependencies ("patch " ++ show h) p >>= \case
--       Left missing -> convertEntities (missing ++ all)
--       Right lookup -> do
--         -- hashId <- Db.saveHashByteString (runV1 h)
--         -- savePatch hashId (Patch.hmap (lookup . V1) p)
--         error "todo"
--         convertEntities rest
-- Branch1 (V1.BranchHash h) ->
--   ifM (existsObjectWithHash h) (convertEntities rest) $ do
--     cb <- loadCausalBranch1 rootDir (V1 h)
--     matchCausalBranch1Dependencies ("branch " ++ show h) cb >>= \case
--       Left missing -> convertEntities (missing ++ all)
--       Right (lookupObject, lookupCausal) -> do
--         convertCausalBranch1 lookupObject lookupCausal cb
--         convertEntities rest


-- -- | load a causal branch raw thingo
-- loadCausalBranch1 ::
--   MonadIO m =>
--   MonadError FatalError m =>
--   CodebasePath ->
--   V1 Hash ->
--   m (V1.Causal.Raw V1.Branch.Raw V1.Branch.Raw)
-- loadCausalBranch1 rootDir h = do
--   let file = V1.branchPath rootDir (V1.RawHash (runV1 h))
--   ifM
--     (doesFileExist file)
--     ( S.getFromFile' (S.V1.getCausal0 S.V1.getRawBranch) file >>= \case
--         Left err -> throwError $ InvalidBranch h
--         Right c0 -> pure c0
--     )
--     (throwError $ MissingBranch h)

-- primaryHashByHash1 :: DB m => V2.ObjectType -> Hash -> m (Maybe Hash)
-- primaryHashByHash1 t h =
--   Db.query sql (t, Base32Hex.fromHash h) <&> \case
--     [Only h] -> Just (Base32Hex.toHash h)
--     [] -> Nothing
--     hs ->
--       error $
--         "hash2ForHash1 " ++ show t ++ " "
--           ++ take 10 (show h)
--           ++ " = "
--           ++ (show . map (take 10 . show)) hs
--   where
--     sql =
--       [here|
--     SELECT v2hash.base32
--     FROM hash AS v2hash
--     INNER JOIN object ON object.primary_hash_id = v2hash.id
--     INNER JOIN hash_object ON object.id = hash_object.object_id
--     INNER JOIN hash AS v1hash ON hash_object.hash_id = v1hash.id
--     WHERE object.type_id = ? AND v1hash.base32 = ?
--   |]

-- loadBranch1 ::
--   forall m.
--   MonadIO m =>
--   MonadError FatalError m =>
--   m V1.Branch.Raw
-- loadBranch1 = error "todo: loadBranch1"

-- --      ifM (not <$> doesFileExist (V1.branchPath root h))
-- --        (throwError $ MissingBranch h)
-- --        (do
-- --          branch1 <- loadBranch1
-- --          )

-- newtype MdValuesR r = MdValues (Set r)

-- deriving via
--   (Set r)
--   instance
--     Hashable r => Hashable (MdValuesR r)

-- -- this is the version we'll hash
-- type RawBranch =
--   RawBranchH
--     (V2.Referent Hash Hash) -- terms
--     (V2.Reference Hash) -- types
--     (V2.Reference Hash) -- metadata
--     (V2 Hash) -- patches
--     (V2 Hash) -- children

-- -- this is the version that closely corresponds to the db schema
-- type RawBranch2S =
--   RawBranchH
--     (V2.Referent Db.ObjectId Db.ObjectId) -- terms
--     (V2.Reference Db.ObjectId) -- types
--     (V2.Reference Db.ObjectId) -- metadata
--     Db.ObjectId -- patches
--     Db.CausalHashId -- children

-- data RawBranchH termRef typeRef mdRef pRef cRef = RawBranch
--   { terms :: Map (NameSegment, termRef) (Set mdRef),
--     types :: Map (NameSegment, typeRef) (Set mdRef),
--     patches :: Map NameSegment pRef,
--     children :: Map NameSegment cRef
--   }

-- type RawCausal = RawCausalH Db.CausalHashId Db.NamespaceHashId

-- data RawCausalH hCausal hValue = RawCausal
--   { causalHash :: hCausal,
--     valueHash :: hValue,
--     parents :: [hCausal]
--   }

-- instance Hashable RawBranch where
--   tokens b =
--     [ H.accumulateToken (terms b),
--       H.accumulateToken (types b),
--       H.accumulateToken (patches b),
--       H.accumulateToken (children b)
--     ]

-- instance Hashable RawCausal where
--   tokens c =
--     [ H.accumulateToken (causalHash c),
--       H.accumulateToken (valueHash c),
--       H.accumulateToken (parents c)
--     ]

getV1RootBranchHash :: MonadIO m => CodebasePath -> m (Maybe V1.BranchHash)
getV1RootBranchHash root = listDirectory (V1.branchHeadDir root) <&> \case
  [single] -> Just . V1.BranchHash . Hash.fromBase32Hex . Base32Hex.UnsafeBase32Hex $ Text.pack single
  _ -> Nothing

-- | Look for an ObjectId corresponding to the provided V1 hash.
--  Returns Left if not found.
lookupObject :: DB m => V1EntityRef -> m (Either V1EntityRef (V1 Hash, (V2 Hash, Db.ObjectId)))
lookupObject r@(runV1 . v1EntityRefToHash -> h) =
  getObjectIdByBase32Hex (Hash.toBase32Hex h) <&> \case
    Nothing -> Left r
    Just i -> Right (V1 h, i)

-- -- | Look for a CausalHashId corresponding to the provided V1 hash.
-- --  Returns Left if not found.
-- lookupCausal :: DB m => V1.Branch.Hash -> m (Either (V1 Hash) (V1 Hash, (V2 Hash, Db.CausalHashId)))
-- lookupCausal (V1.unRawHash -> h) =
--   Db.queryMaybe sql (Only (Base32Hex.fromHash h)) <&> \case
--     Nothing -> Left (V1 h)
--     Just (v2Hash, id) -> Right (V1 h, (Base32Hex.toHash <$> v2Hash, id))
--   where
--     sql =
--       [here|
--     SELECT new_hash.base32, new_hash_id
--     FROM causal_old
--     INNER JOIN hash old_hash ON old_hash_id = old_hash.id
--     INNER JOIN hash new_hash ON new_hash_id = new_hash.id
--     WHERE old_hash.base32 = ?
--   |]

saveTypeBlobForTerm :: DB m => V2.Sqlite.Reference.Id -> V2DiskTypeOfTerm -> m ()
saveTypeBlobForTerm r typ = Db.saveTypeOfTerm r blob
  where
    blob = S.putBytes (S.V2.putType S.V2.putReference S.V2.putSymbol) typ

-- -- | no Maybes here, as all relevant ObjectId can be known in advance
-- saveTypeBlobForReferent :: DB m => V2.ReferentId Db.ObjectId -> Type2S -> m ()
-- saveTypeBlobForReferent r type2s =
--   let blob = S.putBytes (S.V1.putTypeR (V2.putReference V2.putObjectId) putV V2.putUnit) type2s
--    in Db.saveTypeOfReferent r blob

-- | Multiple hashes can map to a single object!
getObjectIdByBase32Hex :: DB m => Base32Hex -> m (Maybe (V2 Hash, Db.ObjectId))
getObjectIdByBase32Hex h =
  fmap (first (V2 . Hash.fromBase32Hex)) <$> Db.objectAndPrimaryHashByAnyHash h

-- augmentLookup :: Ord a => (a -> b) -> Map a b -> a -> b
-- augmentLookup f m a = fromMaybe (f a) (Map.lookup a m)

-- Control.Monad (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
saveReferenceAsReference2 :: DB m => V2.Reference.Reference -> m V2.Sqlite.Reference.ReferenceH
saveReferenceAsReference2 =
  mapMOf V2.Reference.h Db.saveHashHash >=>
  mapMOf V2.Reference.t Db.saveText

-- | load a term component by its hash.
--  A v1 term component is split across an arbitrary number of files.
--  We have to 1) figure out what all the filenames are (preloaded into
--  `termDirComponents`), 2) load them all,
loadTerm1 ::
  MonadIO m =>
  MonadError FatalError m =>
  CodebasePath ->
  Map (V1 Hash) [V1.Reference.Id] ->
  V1 Hash ->
  m [(V1Term, V1Type)]
loadTerm1 rootDir componentsFromDir h = case Map.lookup h componentsFromDir of
  Nothing -> throwError $ MissingTermHash h
  Just set -> case toList set of
    [] -> error "Just [] shouldn't occur here."
    V1.Reference.Id h _i n : _etc -> for [0 .. n -1] \i -> do
      let r = V1.Reference.Id h i n
      term <-
        V1.FC.getTerm rootDir r
          >>= maybe (throwError $ MissingTerm r) pure
      typeOfTerm <-
        V1.FC.getTypeOfTerm rootDir r
          >>= maybe (throwError $ MissingTypeOfTerm r) pure
      pure (term, typeOfTerm)

-- loadDecl1 ::
--   MonadIO m =>
--   MonadError FatalError m =>
--   CodebasePath ->
--   Map (V1 Hash) [Reference.Id] ->
--   V1 Hash ->
--   m [Decl]
-- loadDecl1 rootDir componentsFromDir h = case Map.lookup h componentsFromDir of
--   Nothing -> throwError $ MissingDeclHash h
--   Just set -> case toList set of
--     [] -> error "Just [] shouldn't occur here."
--     Reference.Id h _i n : _etc -> for [0 .. n -1] \i -> do
--       let r = Reference.Id h i n
--       V1.FC.getDecl (S.get fmtV) (S.get fmtA) rootDir r
--         >>= maybe (throwError $ MissingDecl r) pure

-- -- | load a patch
-- loadPatch1 :: (MonadIO m, MonadError FatalError m) => [Char] -> V1 Hash -> m (Patch.Patch Reference)
-- loadPatch1 rootDir h = do
--   let file = V1.editsPath rootDir (runV1 h)
--   ifM
--     (doesFileExist file)
--     ( S.getFromFile' S.V1.getEdits file >>= \case
--         Left _err -> throwError (InvalidPatch h)
--         Right edits -> pure edits
--     )
--     (throwError $ MissingPatch h)

-- 3) figure out what their combined dependencies are
matchTerm1Dependencies ::
  DB m =>
  V1 Hash ->
  [(V1Term, V1Type)] ->
  m (Either [V1EntityRef] (V1 Hash -> V2 Hash, V2 Hash -> Db.ObjectId, Text -> Db.TextId))
matchTerm1Dependencies componentHash tms =
  let -- Get a list of Eithers corresponding to the non-self dependencies of this term.
      lookupDependencyObjects ::
        DB m => (V1Term, V1Type) -> m [Either V1EntityRef (V1 Hash, (V2 Hash, Db.ObjectId))]
      lookupDependencyObjects (term, typeOfTerm) = traverse lookupObject deps
        where
          (termTypeDeps, termTermDeps) =
            partitionEithers
              . map V1.LD.toReference
              . toList
              $ V1.Term.labeledDependencies term
          deps =
            nubOrd $
              [Decl1 (V1 h) | V1.Reference.Derived h _i _n <- toList $ V1.Type.dependencies typeOfTerm]
                <> [Decl1 (V1 h) | V1.Reference.Derived h _i _n <- termTypeDeps]
                <> [ Term1 (V1 h) | V1.Reference.Derived h _i _n <- termTermDeps, h /= runV1 componentHash -- don't include self-refs 😬
                   ]
   in do
        -- check the lefts, if empty then everything is on the right;
        -- else return left.
        (missing, found) <- partitionEithers <$> foldMapM lookupDependencyObjects tms
        -- pure $ case missing of
        --   [] -> Right (makeLookup found $ "term " ++ show componentHash)
        --   missing -> Left missing
        error "todo"

-- matchDecl1Dependencies ::
--   DB m => V1 Hash -> [Decl] -> m (Either [V1EntityRef] (V1 Hash -> Db.ObjectId))
-- matchDecl1Dependencies componentHash decls = error "todo" -- let
-- -- lookupDependencyObjects
-- --   :: DB m => Decl -> m [Either V1EntityRef (V1 Hash, (V2 Hash, Db.ObjectId))]
-- -- lookupDependencyObjects decl = traverse lookupObject . nubOrd $
-- --   [ Decl1 (V1 h) | Reference.Derived h _i _n <- toList (DD.declDependencies decl)
-- --             , V1 h /= componentHash ]
-- -- in do
-- --   (missing, found) <- partitionEithers <$> foldMapM lookupDependencyObjects decls
-- --   pure $ case missing of
-- --     [] -> Right (makeLookup found $ "decl " ++ show componentHash)
-- --     missing -> Left missing

-- matchPatch1Dependencies ::
--   DB m =>
--   String ->
--   Patch ->
--   m (Either [V1EntityRef] (V1 Hash -> (V2 Hash, Db.ObjectId)))
-- matchPatch1Dependencies description (Patch.Patch tms tps) = do
--   deps :: [Either V1EntityRef (V1 Hash, (V2 Hash, Db.ObjectId))] <-
--     traverse lookupObject . nubOrd $
--       [ Term1 (V1 h) | (r, e) <- Relation.toList tms, Reference.Derived h _i _n <- r : TermEdit.references e
--       ]
--         ++ [ Decl1 (V1 h) | (r, e) <- Relation.toList tps, Reference.Derived h _i _n <- r : TypeEdit.references e
--            ]
--   let (missing, found) = partitionEithers deps
--   pure $ case missing of
--     [] -> Right (makeLookup found description)
--     missing -> Left missing

-- -- | multiple lookups needed in converting branch
-- --  data CBDepLookup
-- matchCausalBranch1Dependencies ::
--   DB m =>
--   String ->
--   V1.Causal.Raw V1.Branch.Raw V1.Branch.Raw ->
--   m (Either [V1EntityRef] (V1 Hash -> Db.ObjectId, V1 Hash -> (V2 Hash, Db.CausalHashId)))
-- matchCausalBranch1Dependencies description cb@(V1.Causal.rawHead -> b) = do
--   deps <-
--     traverse lookupObject . nubOrd $
--       -- history
--       [Branch1 h | h <- V1.Causal.rawTails cb]
--         ++
--         -- terms
--         [ Term1 (V1 h)
--           | Referent.Ref (Reference.Derived h _i _n) <-
--               (toList . Relation.dom . Star3.d1 . V1.Branch._termsR) b
--         ]
--         ++ [ Term1 (V1 h)
--              | Referent.Ref (Reference.Derived h _i _n) <-
--                  (toList . Relation.dom . Star3.d1 . V1.Branch._termsR) b
--            ]
--         ++
--         -- term metadata
--         [ Term1 (V1 h)
--           | Reference.Derived h _i _n <-
--               (map snd . toList . Relation.ran . Star3.d3 . V1.Branch._termsR) b
--         ]
--         ++
--         -- types
--         [ Decl1 (V1 h)
--           | Reference.Derived h _i _n <-
--               (toList . Relation.dom . Star3.d1 . V1.Branch._typesR) b
--         ]
--         ++
--         -- type metadata
--         [ Term1 (V1 h)
--           | Reference.Derived h _i _n <-
--               (map snd . toList . Relation.ran . Star3.d3 . V1.Branch._typesR) b
--         ]
--         ++ [Branch1 h | h <- toList (V1.Branch._childrenR b)]
--         ++ [Patch1 (V1 h) | h <- toList (V1.Branch._editsR b)]

--   causalParents <- traverse lookupCausal (V1.Causal.rawTails cb)

--   let (missingEntities, foundObjects) = partitionEithers deps
--   let (missingParents, foundParents) = partitionEithers causalParents

--   error "todo"

-- -- pure $ case missingEntities of
-- --   [] -> Right ( makeLookup foundObjects description
-- --               , makeCausalLookup foundParents description )
-- --   missing -> Left missing

-- makeCausalLookup :: [(V1 Hash, (V2 Hash, Db.CausalHashId))] -> String -> V1 Hash -> (V2 Hash, Db.CausalHashId)
-- makeCausalLookup l description a =
--   let m = Map.fromList l
--    in case Map.lookup a m of
--         Just b -> b
--         Nothing ->
--           error $
--             "Somehow I don't have the CausalHashId for "
--               ++ show (Base32Hex.fromHash (runV1 a))
--               ++ " in the map for "
--               ++ description

makeLookup :: [(V1 Hash, (V2 Hash, Db.ObjectId))] -> String -> V1 Hash -> (V2 Hash, Db.ObjectId)
makeLookup l lookupDescription a =
  let m = Map.fromList l
   in case Map.lookup a m of
        Just b -> b
        Nothing ->
          error $
            "Somehow I don't have the ObjectId for "
              ++ show (Hash.toBase32Hex (runV1 a))
              ++ " in the map for "
              ++ lookupDescription

createTypeSearchIndicesForReferent :: DB m => V2.Sqlite.Referent.Id -> V2HashTypeOfTerm -> m ()
createTypeSearchIndicesForReferent r typ = do
  let typeForIndexing = TypeUtil.removeAllEffectVars typ

  -- add the term to the type index
  typeReferenceForIndexing :: V2.Sqlite.Reference.ReferenceH <-
    saveReferenceAsReference2 (TypeUtil.toReference typeForIndexing)

  Db.addToTypeIndex typeReferenceForIndexing r

  -- add the term to the type mentions index
  typeMentionsForIndexing :: [V2.Sqlite.Reference.ReferenceH] <-
    traverse
      saveReferenceAsReference2
      (toList $ TypeUtil.toReferenceMentions typeForIndexing)

  traverse_ (flip Db.addToTypeMentionsIndex r) typeMentionsForIndexing

-- todo:
createDependencyIndexForTerm :: DB m => V2.Sqlite.Reference.Id -> V2DiskTermComponent -> m ()
createDependencyIndexForTerm tmRef@(V2.Reference.Id selfId i) (V2.TermFormat.LocallyIndexedComponent c) =
-- newtype LocallyIndexedComponent =
--   LocallyIndexedComponent (Vector (LocalIds, Term))
  let
    -- | get the ith element from the term component
    (localIds, localTerm) = c Vector.! fromIntegral i

    -- get the term dependencies as localids
    termRefs :: [V2.TermFormat.TermRef]
    typeRefs :: [V2.TermFormat.TypeRef]
    termLinks :: [V2.Referent.Referent' V2.TermFormat.TermRef V2.TermFormat.TypeRef]
    typeLinks :: [V2.TermFormat.TypeRef]
    (termRefs, typeRefs, termLinks, typeLinks) = TermUtil.dependencies localTerm

    -- and convert them to Reference' TextId ObjectId
    localToDbTextId :: V2.TermFormat.LocalTextId -> Db.TextId
    localToDbTextId (V2.TermFormat.LocalTextId n) =
      V2.LocalIds.textLookup localIds Vector.! fromIntegral n
    localToDbDefnId :: V2.TermFormat.LocalDefnId -> Db.ObjectId
    localToDbDefnId (V2.TermFormat.LocalDefnId n)=
      V2.LocalIds.objectLookup localIds Vector.! fromIntegral n
    localToDbTermRef :: V2.TermFormat.TermRef -> V2.Sqlite.Reference.Reference
    localToDbTermRef = bimap localToDbTextId (maybe selfId localToDbDefnId)
    localToDbTypeRef :: V2.TermFormat.TypeRef -> V2.Sqlite.Reference.Reference
    localToDbTypeRef = bimap localToDbTextId localToDbDefnId
    localFoo :: V2.Referent.Referent' V2.TermFormat.TermRef V2.TermFormat.TypeRef -> V2.Sqlite.Reference.Reference
    localFoo = \case
      V2.Referent.Ref tm -> localToDbTermRef tm
      V2.Referent.Con tp _ -> localToDbTypeRef tp
    dependencies :: [V2.Sqlite.Reference.Reference]
    dependencies =  map localToDbTermRef termRefs
                 <> map localToDbTypeRef typeRefs
                 <> map localFoo termLinks
                 <> map localToDbTypeRef typeLinks
  -- and then add all of these to the dependency index
  in traverse_ (flip Db.addToDependentsIndex tmRef) dependencies

localDefnIdToObjectId :: V2.LocalIds.LocalIds -> V2.TermFormat.LocalDefnId -> Db.ObjectId
localDefnIdToObjectId (V2.LocalIds.LocalIds _t d) (V2.TermFormat.LocalDefnId id) = d Vector.! fromIntegral id

localTextIdToObjectId :: V2.LocalIds.LocalIds -> V2.TermFormat.LocalTextId -> Db.TextId
localTextIdToObjectId (V2.LocalIds.LocalIds t _d) (V2.TermFormat.LocalTextId id) = t Vector.! fromIntegral id

-- createDependencyIndexForDecl :: DB m => V2.ReferenceId Db.ObjectId -> Decl2S -> m ()
-- createDependencyIndexForDecl tmRef@(V2.ReferenceId selfId _i) decl =
--   traverse_ (Db.addDependencyToIndex tmRef)
--     . toList
--     . DD.declDependencies
--     $ DD.rmapDecl (fmap $ fromMaybe selfId) decl

saveTermComponent :: DB m => V1 Hash -> V2 Hash -> V2DiskTermComponent -> m Db.ObjectId
saveTermComponent h1 h2 component = do
  h1Id <- Db.saveHashHash (runV1 h1)
  h2Id <- Db.saveHashHash (runV2 h2)
  o <- Db.saveObject h2Id V2.OT.TermComponent blob
  Db.saveHashObject h1Id o 1
  Db.saveHashObject h2Id o 2
  pure o
  where
    blob = S.putBytes S.V2.putTermFormat (V2.TermFormat.Term component)

saveDeclComponent :: DB m => V1 Hash -> V2 Hash -> V2DiskDeclComponent -> m Db.ObjectId
saveDeclComponent h component = error "todo" -- do
    -- -- o <- Db.saveObject h V2.DeclComponent blob
    -- -- Db.saveHashObject h o 2
    -- -- pure o
    -- -- where
    -- -- blob = S.putBytes (S.V1.putFoldable (V2.putDecl putObjectId putV putA)) component

-- savePatch :: DB m => Db.HashId -> Patch2S -> m ()
-- savePatch h p = do
--   o <- Db.saveObject h V2.Patch (S.putBytes V2.putEdits p)
--   Db.saveHashObject h o 2

-- -- saveBranch :: DB m => Db.HashId ->

-- | Loads a dir with format <root>/base32-encoded-reference.id...
--  into a map from Hash to component references
componentMapForDir :: forall m. MonadIO m => FilePath -> m (Map (V1 Hash) [V1.Reference.Id])
componentMapForDir root = listDirectory root <&> foldl' insert mempty
  where
    insert m filename = case V1.componentIdFromString filename of
      Nothing -> m -- skip silently
      Just r@(V1.Reference.Id h _i _n) ->
        Map.unionWith (<>) m (Map.singleton (V1 h) [r])

existsObjectWithHash :: DB m => Hash -> m Bool
existsObjectWithHash = Db.objectExistsWithHash . Hash.toBase32Hex

convertABT :: forall f v a f' v' a'. Ord v' => (f (V1.ABT.Term f v a) -> f' (V2.ABT.Term f' v' a')) -> (v -> v') -> (a -> a') -> V1.ABT.Term f v a -> V2.ABT.Term f' v' a'
convertABT ff fv fa = goTerm
  where
    goTerm :: V1.ABT.Term f v a -> V2.ABT.Term f' v' a'
    goTerm (V1.ABT.Term vs a out) = V2.ABT.Term (Set.map fv vs) (fa a) (goABT out)
    goABT :: V1.ABT.ABT f v (V1.ABT.Term f v a) -> V2.ABT.ABT f' v' (V2.ABT.Term f' v' a')
    goABT = \case
      V1.ABT.Var v -> V2.ABT.Var (fv v)
      V1.ABT.Cycle t -> V2.ABT.Cycle (goTerm t)
      V1.ABT.Abs v t -> V2.ABT.Abs (fv v) (goTerm t)
      V1.ABT.Tm ft -> V2.ABT.Tm (ff ft)

convertABT0 :: Functor f => V1.ABT.Term f v a -> V2.ABT.Term f v a
convertABT0 (V1.ABT.Term vs a out) = V2.ABT.Term vs a (goABT out) where
  goABT = \case
    V1.ABT.Var v -> V2.ABT.Var v
    V1.ABT.Cycle t -> V2.ABT.Cycle (convertABT0 t)
    V1.ABT.Abs v t -> V2.ABT.Abs v (convertABT0 t)
    V1.ABT.Tm ft -> V2.ABT.Tm (convertABT0 <$> ft)

convertType1to2 :: (V1.Reference.Reference -> r) -> V1.Type.F a -> V2.Type.F' r a
convertType1to2 fr = \case
  V1.Type.Ref r -> V2.Type.Ref (fr r)
  V1.Type.Arrow i o -> V2.Type.Arrow i o
  V1.Type.Ann a k -> V2.Type.Ann a (convertKind k)
  V1.Type.App f x -> V2.Type.App f x
  V1.Type.Effect e b -> V2.Type.Effect e b
  V1.Type.Effects as -> V2.Type.Effects as
  V1.Type.Forall a -> V2.Type.Forall a
  V1.Type.IntroOuter a -> V2.Type.IntroOuter a

convertSymbol :: V1.Symbol.Symbol -> V2.Symbol.Symbol
convertSymbol (V1.Symbol.Symbol id name) = V2.Symbol.Symbol id name

convertKind :: V1.Kind.Kind -> V2.Kind.Kind
convertKind = \case
  V1.Kind.Star -> V2.Kind.Star
  V1.Kind.Arrow i o -> V2.Kind.Arrow (convertKind i) (convertKind o)

type LocalIdState =
  (Map Text V2.TermFormat.LocalTextId, Map (V2 Hash) V2.TermFormat.LocalDefnId)

rewriteType ::
  (V2.Reference.Reference -> State.State LocalIdState V2.TermFormat.TypeRef) ->
  V2HashTypeOfTerm ->
  State LocalIdState V2.TermFormat.Type
rewriteType doRef = V2.ABT.transformM go
  where
    go :: V2.Type.FT k -> State LocalIdState (V2.TermFormat.FT k)
    go = \case
      V2.Type.Ref r -> (V2.Type.Ref <$> doRef r)
      V2.Type.Arrow l r -> pure $ V2.Type.Arrow l r
      V2.Type.Ann a kind -> pure $ V2.Type.Ann a kind
      V2.Type.Effect e b -> pure $ V2.Type.Effect e b
      V2.Type.Effects es -> pure $ V2.Type.Effects es
      V2.Type.Forall a -> pure $ V2.Type.Forall a
      V2.Type.IntroOuter a -> pure $ V2.Type.IntroOuter a

-- | rewrite Vars and Tms 🙃
mapTermToVar ::
  (Foldable f, Functor f, Ord v2) =>
  (v -> v2) ->
  (a -> f (V2.ABT.Term f v a) -> Maybe (V2.ABT.Term f v2 a)) ->
  V2.ABT.Term f v a ->
  V2.ABT.Term f v2 a
mapTermToVar fv ft t@(V2.ABT.Term _ a abt) = case abt of
  V2.ABT.Var v -> V2.ABT.var a (fv v)
  V2.ABT.Cycle body -> V2.ABT.cycle a (mapTermToVar fv ft body)
  V2.ABT.Abs x e -> V2.ABT.abs a (fv x) (mapTermToVar fv ft e)
  V2.ABT.Tm body ->
    case ft a body of
      Nothing -> V2.ABT.tm a (mapTermToVar fv ft `fmap` body)
      Just t' -> t'

mapVarToTerm ::
  (Foldable f, Functor f, Ord v2) =>
  (v -> v2) ->
  (v -> Either (f (V2.ABT.Term f v2 a)) v2) ->
  V2.ABT.Term f v a ->
  V2.ABT.Term f v2 a
mapVarToTerm fAbs fVar t@(V2.ABT.Term _ a abt) = case abt of
  V2.ABT.Var v -> case fVar v of
    Left tm -> V2.ABT.tm a tm
    Right v2 -> V2.ABT.var a v2
  V2.ABT.Cycle body -> V2.ABT.cycle a (mapVarToTerm fAbs fVar body)
  V2.ABT.Abs x e -> V2.ABT.abs a (fAbs x) (mapVarToTerm fAbs fVar e)
  V2.ABT.Tm body -> V2.ABT.tm a (mapVarToTerm fAbs fVar <$> body)

-- | Given a V1 term component, convert and save it to the V2 codebase
-- Pre-requisite: all hash-identified entities in the V1 component have
-- already been converted and added to the V2 codebase, apart from self-
-- references.
convertTerm1 :: DB m => (V1 Hash -> V2 Hash) -> (V2 Hash -> Db.ObjectId) -> (Text -> Db.TextId) -> V1 Hash -> [(V1Term, V1Type)] -> m ()
convertTerm1 lookup1 lookup2 lookupText hash1 v1component = do
  -- construct v2 term component for hashing
  let
      buildTermType2H :: (V1 Hash -> V2 Hash) -> V1Type -> V2HashTypeOfTerm
      buildTermType2H lookup
        = V2.ABT.transform (convertType1to2 goRef)
        . V2.ABT.vmap convertSymbol
        . convertABT0
        where
          goRef = \case
            V1.Reference.Builtin t -> V2.Reference.ReferenceBuiltin t
            V1.Reference.Derived h i _n ->
              V2.Reference.ReferenceDerived
                (V2.Reference.Id (runV2 . lookup $ V1 h) i)
      buildTerm2H :: (V1 Hash -> V2 Hash) -> V1 Hash -> V1Term -> V2HashTerm
      buildTerm2H lookup self = goTerm
        where
          goTerm = convertABT goTermF convertSymbol (const ())
          goTermF :: V1.Term.F V1.Symbol.Symbol () V1Term -> V2.Term.F V2.Symbol.Symbol V2HashTerm
          lookupTermLink = \case
            V1.Referent.Ref r -> V2.Referent.Ref (lookupTerm r)
            V1.Referent.Con r i _ct -> V2.Referent.Con (lookupType r) (fromIntegral i)
          lookupTerm = \case
            V1.Reference.Builtin t -> V2.Reference.ReferenceBuiltin t
            V1.Reference.Derived h i _n ->
              let h' = if V1 h == self then
                         Nothing
                       else Just . runV2 . lookup $ V1 h
              in V2.Reference.ReferenceDerived (V2.Reference.Id h' i)
          lookupType = \case
            V1.Reference.Builtin t -> V2.Reference.ReferenceBuiltin t
            V1.Reference.Derived h i _n ->
              V2.Reference.ReferenceDerived
                (V2.Reference.Id (runV2 . lookup $ V1 h) i)
          goTermF = \case
            V1.Term.Int i -> V2.Term.Int i
            V1.Term.Nat n -> V2.Term.Nat n
            V1.Term.Float f -> V2.Term.Float f
            V1.Term.Boolean b -> V2.Term.Boolean b
            V1.Term.Text t -> V2.Term.Text t
            V1.Term.Char c -> V2.Term.Char c
            V1.Term.Ref r -> V2.Term.Ref (lookupTerm r)
            V1.Term.Constructor r i ->
              V2.Term.Constructor (lookupType r) (fromIntegral i)
            V1.Term.Request r i ->
              V2.Term.Constructor (lookupType r) (fromIntegral i)
            V1.Term.Handle b h -> V2.Term.Handle (goTerm b) (goTerm h)
            V1.Term.App f a -> V2.Term.App (goTerm f) (goTerm a)
            V1.Term.Ann e t -> V2.Term.Ann (goTerm e) (buildTermType2H lookup t)
            V1.Term.Sequence as -> V2.Term.Sequence (goTerm <$> as)
            V1.Term.If c t f -> V2.Term.If (goTerm c) (goTerm t) (goTerm f)
            V1.Term.And a b -> V2.Term.And (goTerm a) (goTerm b)
            V1.Term.Or a b -> V2.Term.Or (goTerm a) (goTerm b)
            V1.Term.Lam a -> V2.Term.Lam (goTerm a)
            V1.Term.LetRec _ bs body -> V2.Term.LetRec (goTerm <$> bs) (goTerm body)
            V1.Term.Let _ b body -> V2.Term.Let (goTerm b) (goTerm body)
            V1.Term.Match e cases -> V2.Term.Match (goTerm e) (goCase <$> cases)
            V1.Term.TermLink r -> V2.Term.TermLink (lookupTermLink r)
            V1.Term.TypeLink r -> V2.Term.TypeLink (lookupType r)
          goCase (V1.Term.MatchCase p g b) =
            V2.Term.MatchCase (goPat p) (goTerm <$> g) (goTerm b)
          goPat = \case
            V1.Pattern.Unbound -> V2.Term.PUnbound
            V1.Pattern.Var -> V2.Term.PVar
            V1.Pattern.Boolean b -> V2.Term.PBoolean b
            V1.Pattern.Int i -> V2.Term.PInt i
            V1.Pattern.Nat n -> V2.Term.PNat n
            V1.Pattern.Float d -> V2.Term.PFloat d
            V1.Pattern.Text t -> V2.Term.PText t
            V1.Pattern.Char c -> V2.Term.PChar c
            V1.Pattern.Constructor r i ps ->
              V2.Term.PConstructor (lookupType r) i (goPat <$> ps)
            V1.Pattern.As p -> V2.Term.PAs (goPat p)
            V1.Pattern.EffectPure p -> V2.Term.PEffectPure (goPat p)
            V1.Pattern.EffectBind r i ps k ->
              V2.Term.PEffectBind (lookupType r) i (goPat <$> ps) (goPat k)
            V1.Pattern.SequenceLiteral ps -> V2.Term.PSequenceLiteral (goPat <$> ps)
            V1.Pattern.SequenceOp p op p2 ->
              V2.Term.PSequenceOp (goPat p) (goSeqOp op) (goPat p2)
          goSeqOp = \case
            V1.Pattern.Cons -> V2.Term.PCons
            V1.Pattern.Snoc -> V2.Term.PSnoc
            V1.Pattern.Concat -> V2.Term.PConcat
      buildTermComponent2S ::
        (V2 Hash -> Db.ObjectId) -> V2 Hash -> V2HashTermComponent -> V2DiskTermComponent
      buildTermComponent2S getId h0 terms =
        let rewrittenTerms :: [(V2.TermFormat.Term, LocalIdState)] =
              map (flip State.runState mempty . rewriteTerm) terms
            rewriteTerm :: V2HashTerm -> State.State LocalIdState V2.TermFormat.Term
            rewriteTerm = V2.ABT.transformM go
              where
                doText :: Text -> State.State LocalIdState V2.TermFormat.LocalTextId
                doText t = do
                  (textMap, objectMap) <- State.get
                  case Map.lookup t textMap of
                    Nothing -> do
                      let id =
                            V2.TermFormat.LocalTextId
                              . fromIntegral
                              $ Map.size textMap
                      State.put (Map.insert t id textMap, objectMap)
                      pure id
                    Just id -> pure id
                doHash :: Hash -> State.State LocalIdState V2.TermFormat.LocalDefnId
                doHash (V2 -> h) = do
                  (textMap, objectMap) <- State.get
                  case Map.lookup h objectMap of
                    Nothing -> do
                      let id =
                            V2.TermFormat.LocalDefnId
                              . fromIntegral
                              $ Map.size objectMap
                      State.put (textMap, Map.insert h id objectMap)
                      pure id
                    Just id -> pure id
                doRecRef :: V2.Reference.Reference' Text (Maybe Hash) -> State.State LocalIdState V2.TermFormat.TermRef
                doRecRef = \case
                  V2.Reference.ReferenceBuiltin t ->
                    V2.Reference.ReferenceBuiltin <$> doText t
                  V2.Reference.ReferenceDerived r ->
                    V2.Reference.ReferenceDerived <$> case r of
                      V2.Reference.Id h i -> V2.Reference.Id <$> traverse doHash h <*> pure i
                doRef :: V2.Reference.Reference -> State.State LocalIdState V2.TermFormat.TypeRef
                doRef = \case
                  V2.Reference.ReferenceBuiltin t ->
                    V2.Reference.ReferenceBuiltin <$> doText t
                  V2.Reference.ReferenceDerived (V2.Reference.Id h i) ->
                    V2.Reference.ReferenceDerived
                      <$> (V2.Reference.Id <$> doHash h <*> pure i)
                go :: V2.Term.F V2.Symbol.Symbol k -> State LocalIdState (V2.TermFormat.F k)
                go = \case
                  V2.Term.Int i -> pure $ V2.Term.Int i
                  V2.Term.Nat n -> pure $ V2.Term.Nat n
                  V2.Term.Float d -> pure $ V2.Term.Float d
                  V2.Term.Boolean b -> pure $ V2.Term.Boolean b
                  V2.Term.Text t -> V2.Term.Text <$> doText t
                  V2.Term.Char c -> pure $ V2.Term.Char c
                  V2.Term.Ref r -> V2.Term.Ref <$> doRecRef r
                  V2.Term.Constructor r cid ->
                    V2.Term.Constructor <$> doRef r <*> pure cid
                  V2.Term.Request r cid -> V2.Term.Request <$> doRef r <*> pure cid
                  V2.Term.Handle e h -> pure $ V2.Term.Handle e h
                  V2.Term.App f a -> pure $ V2.Term.App f a
                  V2.Term.Ann e typ -> V2.Term.Ann e <$> rewriteType doRef typ
            mapToVec :: Ord i => (a -> b) -> Map a i -> Vector b
            mapToVec f = Vector.fromList . map (f . fst) . List.sortOn snd . Map.toList
            stateToIds :: LocalIdState -> V2.LocalIds.LocalIds
            stateToIds (t, o) =
              V2.LocalIds.LocalIds (mapToVec lookupText t) (mapToVec lookup2 o)
            -- state : (Map Text Int, Map Hash Int)
            -- Term.app Nat.+ 7 #8sf73g
            -- ["Nat.+"] [#8sf73g]
            -- [lookupText "Nat.+"] [lookup #8sf73g]
            -- Term.app (Builtin 0) 7 (Hash 0)
         in V2.TermFormat.LocallyIndexedComponent
              . Vector.fromList
              . fmap swap
              . fmap (second stateToIds)
              $ rewrittenTerms
      refToVarTerm ::
        Ord v =>
        V2.ABT.Term (V2.Term.F' text (V2.Reference.Reference' t (Maybe h)) typeRef termLink typeLink vt) v a ->
        V2.ABT.Term (V2.Term.F' text (V2.Reference.Reference' t (Maybe h)) typeRef termLink typeLink vt) (Either (V1 Int) v) a
      refToVarTerm = mapTermToVar Right \a body -> case body of
        V2.Term.Ref (V2.Reference.ReferenceDerived (V2.Reference.Id Nothing i)) ->
          Just $ V2.ABT.var a (Left (V1 (fromIntegral i)))
        _ -> Nothing
      varToRefTerm ::
        (Show v, Ord v) =>
        Map (V1 Int) (V2 Int) ->
        V2.ABT.Term (V2.Term.F' text (V2.Reference.Reference' t (Maybe h)) typeRef termLink typeLink vt) (Either (V1 Int) v) a ->
        V2.ABT.Term (V2.Term.F' text (V2.Reference.Reference' t (Maybe h)) typeRef termLink typeLink vt) v a
      varToRefTerm lookup = mapVarToTerm fromLeft $ mapLeft \(V1 i) ->
        V2.Term.Ref (V2.Reference.Derived Nothing (fromIntegral i))
        where
          fromLeft :: Show a => Either a b -> b
          fromLeft = flip either id \r ->
            error ("encountered a reference pseudovar " ++ show r ++ " in ABT.Abs")
      rehashComponent :: (V1 Hash -> V2 Hash) -> V1 Hash -> [(V1Term, V1Type)] -> (V2 Hash, [V2HashTypeOfTerm], V2HashTermComponent)
      rehashComponent lookup1 hash1 (unzip -> (v1terms, v1types)) =
        let fromLeft = either id (\x -> error $ "impossibly " ++ show x)
         in let indexVars = Left . V1 <$> [0 ..]
                -- create a [(v, V1Term)]
                namedTerms1 :: [(Either (V1 Int) V2.Symbol.Symbol, V1Term)]
                namedTerms1 = zip indexVars v1terms
                -- convert [(v, V1Term)] to [(v, V2Term)]
                namedTerms2 :: [(Either (V1 Int) V2.Symbol.Symbol, V2HashTerm)]
                namedTerms2 = fmap (second (buildTerm2H lookup1 hash1)) namedTerms1
                -- convert the previous to a map
                namedTermMap :: Map (Either (V1 Int) V2.Symbol.Symbol) V2HashTerm
                namedTermMap = Map.fromList namedTerms2
                -- convert the Map v V2Term to (hash, [v, V2Term]) where the list
                -- has a new canonical ordering
                hash2 :: V2 Hash
                v1Index :: [V1 Int]
                -- (h, ([2, 0, 1], [t2, t0, t1])
                (hash2, unzip -> (fmap fromLeft -> v1Index, v2Terms)) =
                  V2.ABT.hashComponent (refToVarTerm <$> namedTermMap)

                -- a mapping from the v1 canonical order to v2 canonical order
                -- Q: Why do you need a map from V1 to V2 Ints?
                -- A: the `v`s embed the component index of a self-reference,
                --
                indexMap :: Map (V1 Int) (V2 Int)
                indexMap = Map.fromList (zip v1Index (V2 <$> [0 :: Int ..]))

                -- convert the V1TypeOfTerm to V2TypeOfTerm,
                -- and permute their order according to indexMap
                convertedTypes, permutedTypes :: [V2HashTypeOfTerm]
                convertedTypes = map (buildTermType2H lookup1) v1types
                -- the first element of v1Index is the V1 index of the first V2 element
                permutedTypes = map (((!!) convertedTypes) . runV1) v1Index
             --
             in (hash2, permutedTypes, varToRefTerm indexMap <$> v2Terms)

      hash2 :: V2 Hash
      v2types :: [V2HashTypeOfTerm]
      v2hashComponent :: V2HashTermComponent
      (hash2, v2types, v2hashComponent) = rehashComponent lookup1 hash1 v1component
      -- construct v2 term component for serializing
      v2diskComponent :: V2DiskTermComponent =
        buildTermComponent2S lookup2 hash2 v2hashComponent

  -- serialize the v2 term component
  componentObjectId :: Db.ObjectId <- saveTermComponent hash1 hash2 v2diskComponent

  -- construct v2 types for each component element, and save the types to the
  -- to the indices
  for_ (zip [0 ..] v2types) $ \(i, type2) -> do
    let r = V2.Reference.Id componentObjectId i
    let rt = V2.Referent.RefId r

    saveTypeBlobForTerm r (buildTermType2S lookupText lookup2 type2)
    createTypeSearchIndicesForReferent rt type2
    createDependencyIndexForTerm r v2diskComponent

convertDecl1 :: DB m => (V1 Hash -> V2 Hash) -> (V2 Hash -> Db.ObjectId) -> V1 Hash -> [V1Decl] -> m ()
convertDecl1 lookup1 lookup2 hash1 v1component = do
  let -- convert constructor type (similar to buildTermType2H)

      -- v2ctorTypes :: [V2TypeOfConstructor] = error "todo"

      -- -- rehash and reorder component
      -- hash2 :: V2 Hash
      -- v2hashComponent :: V2HashDeclComponent
      -- (hash2, v2hashComponent) = rehashComponent lookup1 hash1 v1component
      --   where
      --     -- take a look at existing DataDeclaration.hashDecls

      --     -- |1. for each decl in a component, convert it to the new abt/functor
      --     -- and swap out all its V1 Hashes for V2 Hashes, using `Nothing` for
      --     -- a self-reference hash.
      --     -- 2. lift the vars so that self-references are Left i
      --     -- and local vars are Right Symbol
      --     -- 3. call ABT.hashComponent to get a new hash and a new canonical ordering
      --     -- 4. unlift the vars back, rewrite them to reflect the new canonical ordering
      --     rehashComponent :: (V1 Hash -> V2 Hash) -> V1 Hash -> [V1Decl] -> (V2 Hash, V2HashDeclComponent)
      --     rehashComponent = error "todo"

      -- convert decl component
      -- v2diskComponent :: V2DiskDeclComponent = error "todo"

  -- serialize the v2 decl component
  -- componentObjectId :: Db.ObjectId <- saveDeclComponent hash1 hash2 v2diskComponent

  error "todo: create type indices for each decl in the component"

--   let v2componentI :: [Decl2I] =
--         map (buildDecl2I hash2) v2hashComponent

--   for_ (zip v2componentI [0..]) $ \(decl2, i) -> do
--     let r = V2.ReferenceId componentObjectId i

--     for_ (zip
--             (DD.constructorTypes (DD.asDataDecl decl2))
--             [0..]) $ \(type2, j) -> do
--       let rt = V2.ReferentIdCon r j
--       saveTypeBlobForReferent rt type2
--       createTypeSearchIndicesForReferent rt type1 -- type1 because `find` uses Hashes

--     createDependencyIndexForDecl r decl2

-- convertCausalBranch1 ::
--   DB m =>
--   (V1 Hash -> Db.ObjectId) ->
--   (V1 Hash -> (V2 Hash, Db.CausalHashId)) ->
--   -- -> V1 Hash
--   V1.Causal.Raw V1.Branch.Raw V1.Branch.Raw ->
--   m ()
-- convertCausalBranch1 lookupObject lookupCausal causalBranch1 = error "todo" -- do
-- -- let branch1Hash = V1.currentHash causalBranch1
-- --     rawBranch2 :: RawBranch = convertBranch1 (V1.rawHead causalBranch1)

-- -- -- branch2Id <- Db.saveObject branch1Hash
-- --     branch2Hash :: V2 Hash = H.hash rawBranch2
-- -- lookupObject <- pure ()
-- --     -- rawCausal2 :: RawCausal = convertCausal1
-- -- -- rawBranch2S
-- -- -- rawCausal2 :: RawCausal <- convertCausal1 lookup rawBranch2 (V1.rawTails causalBranch1)

-- -- -- rawBranch2S
-- -- -- saveBranch2 rawBranch2
-- -- -- saveCausal2 rawCausal2
-- -- error "todo"
-- -- -- Due to switching reference types, and reference's hash's having previously
-- -- -- incorporated the extra `len` field, definitions and patches will not end up
-- -- -- having the same hashes as before. :-\
-- -- -- This means we have to hash them again and perhaps store old and new hashes
-- -- -- separately.
-- -- where
-- -- indexBranch2S :: RawBranch -> RawBranch2S
-- -- indexBranch2S b = RawBranch
-- --   (Map.fromList
-- --     [((ns, over Db.referent2Traversal (lookupObject . V1) r),
-- --       Set.map (over Db.reference2Traversal (lookupObject . V1)) mds)
-- --     |((ns, r), mds) <- Map.toList (terms b)])
-- --   (Map.fromList
-- --     [((ns, over Db.reference2Traversal (lookupObject . V1) r),
-- --       Set.map (over Db.reference2Traversal (lookupObject . V1)) mds)
-- --     |((ns, r), mds) <- Map.toList (types b)])
-- --   (Map.fromList [])
-- --   (Map.fromList [])
-- --   -- <$> tms <*> tps <*> pchs <*> chn where
-- --   -- tms = Map.fromList <$> traverse indexTerm (Map.toList (terms b))
-- --   -- indexTerm :: DB m
-- --   --           => ((NameSegment, Db.Referent2 Hash), Set (V2.Reference Hash))
-- --   --           -> m ((NameSegment, Db.Referent2 Db.ObjectId), Set (V2.Reference Db.ObjectId))
-- --   -- indexTerm ((ns, r), mds) = (,) <$> k <*> v where
-- --   --   k = (ns, over Db.referentTraversal lookupObject r)
-- --   --   v = Set.map

-- -- convertBranch1 :: V1.Branch.Raw -> RawBranch
-- -- convertBranch1 b = RawBranch
-- --   -- terms
-- --   (Map.fromList
-- --       [ ((ns, over Db.referentTraversal id r), mdSet)
-- --       | (r, ns) <- Relation.toList . Star3.d1 $ V1.Branch._termsR b
-- --       , let mdSet :: Set (V2.Reference Hash)
-- --             mdSet = Set.fromList
-- --                   . fmap (over Db.referenceTraversal id . snd)
-- --                   . Set.toList
-- --                   . Relation.lookupDom r
-- --                   . Star3.d3
-- --                   $ V1.Branch._termsR b
-- --       ])
-- --   -- types
-- --   (Map.fromList
-- --       [ ((ns, over Db.referenceTraversal id r), mdSet)
-- --       | (r, ns) <- Relation.toList . Star3.d1 $ V1.Branch._typesR b
-- --       , let mdSet :: Set (V2.Reference Hash)
-- --             mdSet = Set.fromList
-- --                   . fmap (over Db.referenceTraversal id . snd)
-- --                   . Set.toList
-- --                   . Relation.lookupDom r
-- --                   . Star3.d3
-- --                   $ V1.Branch._typesR b
-- --       ])
-- --   -- patches
-- --   (V1.Branch._editsR b)
-- --   -- children
-- --   (runV2 . fst . lookupCausal . V1 . V1.unRawHash <$> V1.Branch._childrenR b)

-- voidTermAnnotations ::
--   V1.TermR tmRef tpRef tmLink tpLink (V1.TypeR tpRef vt at) blankRepr ap v a ->
--   V1.TermR tmRef tpRef tmLink tpLink (V1.TypeR tpRef vt ()) Void () v ()
-- voidTermAnnotations =
--   void . Term.extraMap id id id id void undefined (const ())

-- ----- graveyard
-- ---- |True if `h` (just the hash!) is interned in the DB
-- --knownHash :: DB m => Hash -> m Bool
-- --knownHash h = anyExists $ Db.query sql [Base32Hex.fromHash h] where
-- --  sql = [here| SELECT 1 FROM hash WHERE base32 = ? |]

-- --saveReference :: DB m => ReferenceH h -> m Db.ReferenceId
-- --saveReference r = insert r >> fmap fromJust (loadReference r) where
-- --  insert = \case
-- --    Reference.Builtin t -> execute sql (Just t, Nothing)
-- --    Reference.DerivedId idH -> do
-- --      rdId <- saveReferenceDerived idH
-- --      Db.execute sql (Nothing, Just rdId)
-- --  sql = [here|
-- --    INSERT OR IGNORE INTO reference (builtin, reference_derived_id)
-- --    VALUES (?, ?)
-- --  |]

-- --loadReferenceByHashId :: DB m => ReferenceH HashId -> m (Maybe ReferenceId)
-- --loadReferenceByHashId = \case
-- --  Reference.Builtin t -> queryMaybe sqlb (Only t)
-- --  Reference.DerivedId idH ->
-- --    loadReferenceDerivedByHashId idH >>= \case
-- --      Nothing -> pure Nothing
-- --      Just rdId -> queryMaybe sqld (Only rdId)
-- --  where
-- --  sqlb = [here| SELECT id FROM reference WHERE builtin = ? |]
-- --  sqld = [here| SELECT id FROM reference WHERE reference_derived_id = ? |]

-- --saveReferenceDerived :: DB m => Reference.Id -> m Db.ReferenceDerivedId
-- --saveReferenceDerived r@(Reference.Id h i _n) = do
-- --  hashId <- saveHashByteString h
-- --  saveReferenceDerivedByHashId (Reference.IdH hashId i _n)
-- --
-- --saveReferenceDerivedByHashId :: DB m => Reference.IdH Db.HashId -> m Db.ReferenceDerivedId
-- --saveReferenceDerivedByHashId r@(Reference.IdH hashId i _n) =
-- --  insert hashId i >> fmap fromJust (loadReferenceDerivedByHashId r) where
-- --  insert h i = liftIO $ execute sql (h, i) where
-- --    sql = [here|
-- --      INSERT OR IGNORE INTO reference_derived (hash_id, component_index)
-- --      VALUES (?, ?)
-- --    |]
-- --
-- --loadReferenceDerivedByHashId :: DB m => Reference.IdH Db.HashId -> m (Maybe Db.ReferenceDerivedId)
-- --loadReferenceDerivedByHashId (Reference.IdH h i _n) =
-- --  queryMaybe sql (h, i) where
-- --  sql = [here|
-- --    SELECT id FROM reference_derived
-- --    WHERE hash_id = ? AND component_index = ?
-- --  |]

-- --saveReferentDerived :: DB m => Referent.Id -> m ReferentDerivedId
-- --saveReferentDerived = error "todo"
-- --loadReferentDerived :: DB m => Referent.Id -> m (Maybe ReferentDerivedId)
-- --loadReferentDerived = error "todo"
-- --
-- --saveReferentDerivedByReferenceDerivedId :: DB m => Referent' ReferenceDerivedId -> m ReferentDerivedId
-- --saveReferentDerivedByReferenceDerivedId r = do
-- --  liftIO $ execute sql r
-- --  fmap fromJust (loadReferenceDerivedByReferenceDerivedId r)
-- --  where
-- --  sql = [here|
-- --    INSERT OR IGNORE INTO referent_derived
-- --      (reference_derived_id, constructor_id, constructor_type)
-- --    VALUES (?, ?, ?)
-- --  |]
-- --loadReferentDerivedByReferenceDerivedId :: DB m => Referent' ReferenceDerivedId -> m (Maybe ReferentDerivedId)
-- --loadReferentDerivedByReferenceDerivedId r =  queryMaybe . query sql r where
-- --  sql = [here|
-- --    SELECT id FROM referent_derived
-- --    WHERE reference_derived_id = ?
-- --      AND constructor_id = ?
-- --      AND constructor_type = ?
-- --  |]

-- buildTerm2H :: (V1 Hash -> V2 Hash) -> V1 Hash -> Term -> Term2 Hash
-- buildTerm2H lookup hash1 =
--   voidTermAnnotations . Term.rmap
--     (over Db.referenceTraversal (fmap runV2 . lookupTerm . V1))
--     (over Db.referenceTraversal (runV2 . lookupType . V1))
--     ( over Db.referent2ConTraversal (runV2 . lookupType . V1)
--         . over Db.referentRefTraversal (fmap runV2 . lookupTerm . V1)
--     )
--   where
--     lookupTerm :: V1 Hash -> Maybe (V2 Hash)
--     lookupTerm h | h == hash1 = Nothing
--     lookupTerm h = Just (lookup h)
--     lookupType :: V1 Hash -> V2 Hash
--     lookupType = lookup

-- buildTerm2S :: (V1 Hash -> Db.ObjectId) -> V1 Hash -> Term -> Term2 Db.ObjectId
-- buildTerm2S lookup hash1 =
--   voidTermAnnotations . Term.rmap
--     (over Db.referenceTraversal (lookupTerm . V1))
--     (over Db.referenceTraversal (lookupType . V1))
--     ( over Db.referent2ConTraversal (lookupType . V1)
--         . over Db.referentRefTraversal (lookupTerm . V1)
--     )
--   where
--     lookupTerm :: V1 Hash -> Maybe Db.ObjectId
--     lookupTerm h | h == hash1 = Nothing
--     lookupTerm h = Just (lookup h)
--     lookupType :: V1 Hash -> Db.ObjectId
--     lookupType = lookup

buildTermType2S :: (Text -> Db.TextId) -> (V2 Hash -> Db.ObjectId) -> V2HashTypeOfTerm -> V2DiskTypeOfTerm
buildTermType2S lookupText lookup2 = V2.Type.rmap
  (over V2.Reference.t lookupText . over V2.Reference.h (lookup2 . V2))

-- buildDecl2H :: (V1 Hash -> V2 Hash) -> V1 Hash -> Decl -> Decl2 Hash
-- buildDecl2H lookup =
--   void . DD.rmapDecl (over Db.referenceTraversal (fmap runV2 . lookup' . V1))
--   where
--     lookup' :: V1 Hash -> Maybe (V2 hash)
--     lookup' h | h == hash1 = Nothing
--     lookup' h = Just (lookup h)

-- buildDecl2I :: V2 Hash -> Decl2 Hash -> Decl2I
-- buildDecl2I self =
--   DD.rmapDecl (over Db.reference2Traversal (fmap runV2 . fromMaybe self . V2))
