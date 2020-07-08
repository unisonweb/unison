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

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Unison.Codebase2a.SyncV1V2 where

import Unison.Prelude hiding (Map)

import           Control.Lens ()
import           Control.Monad.State.Strict     ( MonadState, evalStateT )
--import           Control.Monad.Writer.Strict    ( MonadWriter, execWriterT )
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
--import qualified Data.Set as Set
--import           Data.Set (Set)
import           Data.List.Extra (nubOrd)
import Data.Maybe (fromJust)
--import Safe (headMay)
import qualified Data.Text as Text
--import qualified Control.Monad.Writer.Strict   as Writer
import qualified Unison.Codebase.Serialization as S
--import qualified Unison.Codebase                  as V1
import Unison.Codebase (CodebasePath)
import qualified Unison.Codebase.Serialization.V1 as S.V1
--import qualified Unison.Codebase.FileCodebase     as V1
import qualified Unison.Codebase.FileCodebase.Common as V1
import qualified Unison.Codebase.FileCodebase.Common as V1.FC
--import qualified Unison.Codebase.FileCodebase.Common as V1.Common
--import qualified Unison.Codebase2a.Serialization.V2 as V2
import qualified Unison.Codebase2a.ObjectType as V2
--import qualified Unison.Codebase.FileCodebase.SlimCopyRegenerateIndex as V1Sync
--import qualified Unison.Codebase.Branch.Dependencies as V1
import qualified Unison.Codebase.Branch as V1.Branch
import qualified Unison.Codebase.Causal as V1
import qualified Unison.Codebase.Causal as V1.Causal
--import Unison.Codebase.SyncMode (SyncMode)
--import qualified Unison.Codebase.SyncMode as SyncMode
import Unison.Hash (Hash)
import qualified Unison.Hash as Hash
import qualified Unison.Hashable as H
import Data.String.Here.Uninterpolated (here)
import Unison.Reference (ReferenceH)
import qualified Unison.Reference as Reference
import Unison.Referent (ReferentH)
import Unison.NameSegment (NameSegment)
import Database.SQLite.Simple
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

newtype PatchHash = PatchHash Hash
newtype NamespaceHash = NamespaceHash Hash
newtype CausalHash =  CausalHash Hash
newtype HashId = HashId Int deriving FromField via Int
newtype ObjectId = ObjectId Int deriving FromField via Int
newtype CausalId = CausalId Int
newtype NamespaceId = NamespaceId Int

-- let's put this in a new module and leave the constructor private
-- so that conversions can be trusted / safe
newtype Base32Hex = Base32Hex Text
  deriving Show via Text
  deriving ToField via Text
  deriving FromField via Text
hashToBase32Hex :: Hash -> Base32Hex
hashToBase32Hex = Base32Hex . Hash.base32Hex
hashFromBase32Hex :: Base32Hex -> Hash
hashFromBase32Hex (Base32Hex t) = Hash.unsafeFromBase32Hex t

newtype Base32HexPrefix = Base32HexPrefix Text
  deriving Show via Text
  deriving ToField via Text
  deriving FromField via Text

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
                | MissingTypeOfTerm Reference.Id
                | MissingDecl Reference.Id
                | InvalidBranch Hash
                | InvalidPatch Hash
                | InvalidTerm Reference.Id
                | InvalidTypeOfTerm Reference.Id
                | InvalidDecl Reference.Id

-- todo: this just converts a whole codebase, which we need to do locally \
--       but we also want some code that imports just a particular sub-branch.
syncV1V2 :: forall v a. Var v
         => Connection
         -> CodebasePath
         -> S.Format v
         -> S.Format a
         -> IO (Either FatalError ())
syncV1V2 conn rootDir fmtV fmtA = withTransaction conn . runExceptT $ do
  v1RootHash <- getV1RootBranchHash rootDir >>= maybe (throwError NoRootBranch) pure
  -- termComponentMap <- componentMapForDir (V1.termsDir rootDir)
  -- declComponentMap <- componentMapForDir (V1.typesDir rootDir)
  convertEntities
    -- termComponentMap
    -- declComponentMap
    [Branch1 v1RootHash]
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
  convertEntities :: forall m. MonadIO m
                  => MonadError FatalError m
                  => [V1EntityRef]
                  -> m ()
  convertEntities [] = pure ()
  convertEntities all@(h : rest) = do
    termDirComponents <- componentMapForDir (V1.termsDir rootDir)
    declsDirComponents <- componentMapForDir (V1.typesDir rootDir)
    case h of
      Branch1 (V1.unRawHash -> h) -> error "todo"
        -- go h loadCausalBranch1 matchCausalBranch1Dependencies convertCausalBranch1
      Patch1 h -> error "todo" -- go h loadPatch1 matchPatch1Dependencies convertPatch1
      Term1 h ->
        ifM (knownHash h) (convertEntities rest) $ do
          e <- loadTerm1 termDirComponents h
          matchTerm1Dependencies e >>= \case
            Left missing -> convertEntities (missing ++ all)
            Right lookup -> do
              convertTerm1 lookup h e
              convertEntities rest
      Decl1 h -> error "todo" -- go h loadDecl1 matchDecl1Dependencies convertDecl1
    where
    -- |load a term component by its hash.
    -- A v1 term component is split across an arbitrary number of files.
    -- We have to 1) figure out what all the filenames are (preloaded into
    -- `termDirComponents`), 2) load them all,
    loadTerm1 :: Map Hash [Reference.Id]
              -> Hash
              -> m [(Term.Term v a, Type.Type v a)]
    loadTerm1 components h = case Map.lookup h components of
      Nothing -> throwError $ MissingTerm (Reference.Id h 0 0)
      Just set -> case toList set of
        [] -> error "that's weird"
        Reference.Id h _i n : _etc -> for [0..n-1] \i -> do
          let r = Reference.Id h i n
          term <- V1.FC.getTerm (S.get fmtV) (S.get fmtA) rootDir r
            >>= maybe (throwError $ MissingTerm r) pure
          typeOfTerm <- V1.FC.getTypeOfTerm (S.get fmtV) (S.get fmtA) rootDir r
            >>= maybe (throwError $ MissingTypeOfTerm r) pure
          pure (term, typeOfTerm)
    -- 3) figure out what their combined dependencies are
    matchTerm1Dependencies ::
      [(Term.Term v a, Type.Type v a)]
      -> m (Either [V1EntityRef] (Hash -> ObjectId))
    matchTerm1Dependencies tms = do
      (missing, found) <- fmap partitionEithers $ foldMapM go tms
      pure $ case missing of
        [] -> Right ((Map.fromList found) Map.!)
        missing -> Left missing
      where
      go :: (Term.Term v a, Type.Type v a) -> m [Either V1EntityRef (Hash, ObjectId)]
      go (term, typeOfTerm) = let
        (termTypeDeps, termTermDeps) =
          partitionEithers
            . map LD.toReference
            . toList
            $ Term.labeledDependencies term
        deps = nubOrd $
          [ Decl1 h | Reference.Derived h _i _n <- toList $ Type.dependencies typeOfTerm ] <>
          [ Decl1 h | Reference.Derived h _i _n <- termTypeDeps ] <>
          [ Term1 h | Reference.Derived h _i _n <- termTermDeps ]
        -- for each dep, look for a corresponding id in the db.
        -- if it has one, right; else left.
        -- check the lefts, if empty then everything is on the right;
        -- else return left.
        get :: V1EntityRef -> m (Either V1EntityRef (Hash, ObjectId))
        get ref@(v1EntityRefToHash -> h) =
          maybe (Left ref) (\i -> Right (h,i)) <$>
            objectIdByBase32Hex @m  (hashToBase32Hex h)
        in traverse get deps

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

  -- |load a patch
  loadPatch1 h = do
    let file = V1.editsPath rootDir h
    ifM (doesFileExist file)
      (S.getFromFile' S.V1.getEdits file >>= \case
        Left  _err   -> throwError (InvalidPatch h)
        Right edits -> pure edits)
      (throwError $ MissingPatch h)

  loadDecl1 = error "todo"
  componentMapForDir :: MonadIO m => FilePath -> m (Map Hash [Reference.Id])
  componentMapForDir root = listDirectory root <&> foldl' insert mempty
    where
    insert m filename = case V1.componentIdFromString filename of
      Nothing -> m -- skip silently
      Just r@(Reference.Id h _i _n) ->
        Map.unionWith (<>) m (Map.singleton h [r])

  -- getComponentForHash :: S.Get a -> FilePath -> Hash -> [a]
  -- getComponentForHash fileList get h = do
  --   allFiles <- listDirectory rootDir
  --   error "todo"

  matchCausalBranch1Dependencies = error "todo"
  matchPatch1Dependencies = error "todo"
  matchDecl1Dependencies = error "todo"
  convertCausalBranch1
    :: MonadIO m
    => MonadError FatalError m
    => (Hash -> V2EntityRef)
    -> (V1.Causal.Raw V1.Branch.Raw V1.Branch.Raw)
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
  convertPatch1 lookup patch1 = do
    rawPatch2 <- error "todo: convert patch" lookup patch1
    savePatch2 rawPatch2
    where savePatch2 = error "todo"
  convertTerm1 :: (MonadIO m, Var v)
    => (Hash -> ObjectId) -> Hash -> [(Term.Term v a, Type.Type v a)] -> m ()
  convertTerm1 lookup' hash1 component1 = do
    -- todo: Self-references are going to fail. To reference a hash, there needs
    -- to be an object id for it.  Or just reference the HashId instead of
    -- ObjectId.  Maybe it's fine to references hashes instead of objects,
    -- since the hashes are based on hashes.
    _ <- error "todo: amend lookup with self-reference id if we are still doing that?"
    let component2 = map convertPair component1
    error "saveTerm2 component2"
    where
    convertPair (tm, tp) = (Term.hmap lookup tm, Type.hmap lookup tp)
--    saveTerm2 component = liftIO $ execute conn sql (Only blob)
--      where
--      blob = S.putBytes (S.V1.putFoldable (S.V1.putTerm (S.put fmtV) (S.put fmtA)) component)
--      sql = [here|
--        INSERT INTO hash_object ()
--        VALUES (?,?,?)
--      |]
  convertDecl1 lookup decl1 = do
    decl2 <- error "todo" lookup decl1
    saveDecl2 decl2
    where saveDecl2 = error "todo"

  getOrInsertHash :: forall m. MonadIO m => Base32Hex -> m HashId
  getOrInsertHash h = insert >> fmap fromJust select
    where
    insert = liftIO $ execute conn sql (Only h) where
      sql = [here| INSERT OR IGNORE INTO hash (base32) VALUES (?) |]
    select = queryMaybe $ query conn sql (Only h) where
      sql = [here| SELECT id FROM hash WHERE base32 = ? |]

  queryList :: forall m a. MonadIO m => IO [Only a] -> m [a]
  queryList = liftIO . fmap (map fromOnly)
  queryMaybe :: forall m a. MonadIO m => IO [Only a] -> m (Maybe a)
  queryMaybe = liftIO . fmap (fmap fromOnly . headMay)

  primaryHashByHash1 :: MonadIO m => V2.ObjectType -> Hash -> m (Maybe Hash)
  primaryHashByHash1 t h =
    liftIO $ query conn sql (t, hashToBase32Hex h) <&> \case
      [Only h] -> Just (hashFromBase32Hex h)
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

  -- objectIdsByBase32Hex :: MonadIO m => [Base32Hex] -> m (Map Base32Hex ObjectId)
  -- objectIdsByBase32Hex hs fmap Map.fromList $ for hs objectIdByBase32Hex
  objectIdByBase32Hex :: MonadIO m => Base32Hex -> m (Maybe ObjectId)
  objectIdByBase32Hex h = getMaybe $ query conn sql (Only h)
    where
    sql = [here|
      SELECT object.id
      FROM hash
      INNER JOIN hash_object ON hash_object.hash_id = hash.id
      INNER JOIN object ON hash_object.object_id = object.id
      WHERE hash.base32 = ?
    |]
  -- objectIdsByHash :: MonadIO m => [Hash] -> m (Map Hash ObjectId)
  -- objectIdsByHash hs =
  --   Map.mapKeys hashFromBase32Hex <$>
  --     objectIdsByBase32Hex (hashToBase32Hex <$> hs)

  knownHash :: MonadIO m => Hash -> m Bool
  knownHash h = anyExists $ query conn sql [hashToBase32Hex h] where
    sql = [here| SELECT 1 FROM hash WHERE base32 = ? |]
  existsObjectWithHash :: MonadIO m => Hash -> m Bool
  existsObjectWithHash h = anyExists $ query conn sql [hashToBase32Hex h] where
    sql = [here|
      SELECT 1
      FROM hash INNER JOIN hash_object ON hash.id = hash_object.hash_id
      WHERE base32 = ?
    |]
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

--data RawBranch1 = RawBranch1 { }

getV1RootBranchHash :: MonadIO m => CodebasePath -> m (Maybe V1.Branch.Hash)
getV1RootBranchHash root = listDirectory (V1.branchHeadDir root) <&> \case
  [single] -> Just . V1.Branch.Hash . Hash.unsafeFromBase32Hex $ Text.pack single
  _ -> Nothing


--- sqlite utils
getMaybe :: MonadIO m => IO [Only a] -> m (Maybe a)
getMaybe = liftIO . fmap (fmap fromOnly . headMay)
anyExists :: MonadIO m => IO [Only Int] -> m Bool
anyExists = liftIO . fmap (not . null . map (fromOnly @Int))
