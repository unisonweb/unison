{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.FileCodebase.CopyFilterIndex (syncToDirectory) where

import Unison.Prelude

import qualified Data.Text                     as Text
import           UnliftIO.Directory             ( doesFileExist )
import           System.FilePath                ( FilePath
                                                , (</>)
                                                )
import           Unison.Codebase                ( CodebasePath )
import qualified Unison.Codebase.Causal        as Causal
import           Unison.Codebase.Branch         ( Branch )
import qualified Unison.Codebase.Branch        as Branch
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import qualified Unison.Referent               as Referent
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Monoid as Monoid
import qualified Unison.Util.Star3             as Star3
import qualified Unison.Util.Relation          as Relation
import           Unison.Util.TransitiveClosure as TC
import Control.Monad.State (StateT)
import qualified Control.Monad.State           as State
import qualified Control.Monad.Writer          as Writer
import Control.Lens
import Unison.Util.Relation (Relation)
import Data.Monoid.Generic
import Unison.Codebase.FileCodebase.Common
import GHC.Generics (Generic)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Unison.Util.Set as Set
import Unison.Codebase.Patch (Patch)

data SyncedEntities = SyncedEntities
  { _syncedTerms     :: Set Reference.Id
  , _syncedDecls     :: Set Reference.Id
  , _syncedEdits     :: Set Branch.EditHash
  } deriving Generic
  deriving Semigroup via GenericSemigroup SyncedEntities
  deriving Monoid via GenericMonoid SyncedEntities

makeLenses ''SyncedEntities

syncToDirectory :: MonadIO m => v -> a
                -> CodebasePath -> CodebasePath -> Branch m -> m ()
syncToDirectory _ _ = syncToDirectory'

-- Copy all dependent codebase elements of `branch` from `srcPath` into
-- `destPath`.
--
-- As a refresher, in the normal course of using `ucm` and updating the
-- namespace, we call Branch.sync to write the updated root to disk.
-- Branch.sync takes a few parameters:
--  - `exists`: checks if a branch file already exists in the codebase,
--              so we can skip it and the rest of its history.
--  - `serializeRaw`: given a Branch.Hash, writes a Branch.Raw to disk
--  - `serializeEdits`: given an EditsHash, writes an `m Patch` to disk
--
-- In this module, our `serializeRaw` (called `copyRawBranch`) works by
-- copying branch, term, decl, and edits files.  It only serializes a
-- branch from memory if it doesn't exist on disk; e.g. a merge root node
--
-- Our `serializeEdits` ignores the `m Patch` it's given, and assumes it can
-- file-copy one from disk based on the provided EditsHash.
--
-- The dependents, type, and type mention indices are populated by tracking
-- which entities are copied (using a `SyncedEntities`), loading the entirety
-- of the source index into memory, filtering it according to the
-- `SyncedEntities`, and writing the relevant parts to the destination.
--
-- No definitions are deserialized or serialized during this sync.
syncToDirectory' :: forall m
  . MonadIO m
  => CodebasePath
  -> CodebasePath
  -> Branch m
  -> m ()
syncToDirectory' srcPath destPath newRemoteRoot =
  flip State.evalStateT mempty $ do
    Branch.sync
      (hashExists destPath)
      copyRawBranch
      (flip copyEdits)
      (Branch.transform lift newRemoteRoot)
    x <- use syncedTerms
    y <- use syncedDecls
    dependentsIndex <- loadDependentsDir srcPath
    copyTransitiveDependencies (x <> y) dependentsIndex
    -- reload x and y as they may have grown while copying transitive deps
    x <- use syncedTerms
    y <- use syncedDecls
    copyDependentsIndex (x <> y) dependentsIndex
    -- in processing x and y, we distill them to a direct form:
    knownReferents <- copyTypeIndex x y
    copyTypeMentionsIndex knownReferents
  where
  -- Loads the entire dependents index from disk, copies the appropriate subset
  -- to `destPath`, then uses it to compute transitive dependents, and copy
  -- those definitions, if needed, to `destPath`.  We use State to avoid copying
  -- anything twice.
  copyDependentsIndex :: forall m. MonadIO m
    => Set Reference.Id -> Relation Reference Reference.Id -> m ()
  copyDependentsIndex ids dependentsIndex =
    copyIndexHelper
      (const (pure dependentsIndex))
      (\k v -> touchIdFile v (dependentsDir destPath k))
      ids
  copyTransitiveDependencies ids dependentsIndex =
    for_ transitiveDependencies $ \r ->
      ifM (isTerm r) (copyTerm r) $
        ifM (isDecl r) (copyDecl r) $
          liftIO . putStrLn $
            "❗️ I was trying to copy the definition of " ++ show r ++
            ", which is a dependency of " ++
            (show . toList) (Relation.lookupRan r dependentsIndex) ++
            ", but I couldn't find it as a type _or_ a term."
    where
    dependenciesOf r =
      Set.mapMaybe Reference.toId (Relation.lookupRan r dependentsIndex)
    transitiveDependencies = TC.transitiveClosure' dependenciesOf ids
    isTerm = doesFileExist . termPath srcPath
    isDecl = doesFileExist . declPath srcPath

  copyTypeIndex :: forall m. MonadIO m
                => Set Reference.Id -> Set Reference.Id -> m (Set Referent.Id)
  copyTypeIndex termIds declIds = do
    -- load the type index, collect all the referents that correspond to the input sets
    index <- loadTypeIndexDir srcPath
    let needed = \case
          Referent.Ref' r     -> Set.member r termIds
          Referent.Con' r _ _ -> Set.member r declIds
    Writer.execWriterT $
      for_ (Relation.toList index) $ \(k, v) -> when (needed v) $ do
        liftIO $ touchReferentIdFile v (typeIndexDir destPath k)
        Writer.tell $ Set.singleton v
  copyTypeMentionsIndex :: forall m. MonadIO m => Set Referent.Id -> m ()
  copyTypeMentionsIndex = copyIndexHelper loadTypeMentionsDir $
    \k v -> touchReferentIdFile v (typeMentionsIndexDir destPath k)

  -- | loads an index using IO, restrict it to `neededRange`, and run the `touch`
  -- action on each of the resulting pairs.
  copyIndexHelper :: forall m d r. MonadIO m
                  => (Ord d, Ord r)
                  => (CodebasePath -> m (Relation d r))
                  -> (d -> r -> m ())
                  -> Set r
                  -> m ()
  copyIndexHelper loadIndex touchIndexFile neededRange = do
    available <- loadIndex srcPath
    let needed = Relation.restrictRan available neededRange
    traverse_ (uncurry touchIndexFile) (Relation.toList needed)

  -- | This handler copies a given branch history node, and any term files, decl
  -- files, and patch files it finds referenced by it. It records the DerivedIds
  -- it encounters into `typeIndexQueue` for filter/copying the type indices
  -- later, as well as into `syncedDecls` and `syncedTerms` for filtering/copying
  -- the dependents index later.
  -- If the branch isn't known on disk, then it's probably a new merge node, and
  -- is serialized from memory like normal.
  copyRawBranch :: Branch.Hash -> Causal.Raw Branch.Raw Branch.Raw -> StateT SyncedEntities m ()
  copyRawBranch rh rc = unlessM (hashExists destPath rh) $ do
    copyReferencedDefns $ Causal.rawHead rc
    copyOrReserializeRawBranch rh rc
    where
    copyOrReserializeRawBranch rh rc =
      ifM (doesFileExist (branchPath srcPath rh))
        (copyFileWithParents (branchPath srcPath rh) (branchPath destPath rh))
        (serializeRawBranch destPath rh rc)
    copyReferencedDefns :: Branch.Raw -> StateT SyncedEntities m ()
    copyReferencedDefns (Branch.Raw terms types _ _) = do
      -- Copy decls and enqueue Ids for dependents indexing
      traverse_ copyDecl
        . mapMaybe Reference.toId
        . toList
        $ Star3.fact types
      -- Copy term definitions, enqueue Ids for dependents indexing,
      traverse_ copyTerm
        . mapMaybe Reference.toId
        . mapMaybe Referent.toTermReference
        . toList
        $ Star3.fact terms
  copyDecl, copyTerm :: Reference.Id -> StateT SyncedEntities m ()
  copyDecl = doFileOnce destPath syncedDecls declPath $
    \i -> copyFileWithParents (declPath srcPath i) (declPath destPath i)
  copyTerm = doFileOnce destPath syncedTerms termPath $
    \i -> do
      copyFileWithParents (termPath srcPath i) (termPath destPath i) -- compiled.ub
      copyFileWithParents (typePath srcPath i) (typePath destPath i) -- type.ub
      whenM (doesFileExist $ watchPath srcPath UF.TestWatch i) $
        copyFileWithParents (watchPath srcPath UF.TestWatch i)
                            (watchPath destPath UF.TestWatch i)
  copyEdits :: StateT SyncedEntities m Patch -> Branch.EditHash -> StateT SyncedEntities m ()
  copyEdits p = doFileOnce destPath syncedEdits editsPath $ \h -> do
    ifM (doesFileExist (editsPath srcPath h))
        (copyFileWithParents (editsPath srcPath h) (editsPath destPath h))
        (serializeEdits destPath h p)

-- Relation Dependency Dependent, e.g. [(List.foldLeft, List.reverse)]
-- codebasePath / "dependents" / "_builtin" / Nat / yourFunction
-- codebasePath / "dependents" / <derivedId> / yourFunction
loadDependentsDir :: MonadIO m => CodebasePath -> m (Relation Reference Reference.Id)
loadDependentsDir = loadIndex (Reference.idFromText . Text.pack) . dependentsDir'

-- | Helper for loading relations from disk like the previous one.
-- you just provide a function that can turn `yourFunction` into a `Maybe k`
-- `Nothing`s are ignored.
loadIndex :: forall m k. (MonadIO m, Ord k)
           => (String -> Maybe k) -> FilePath -> m (Relation Reference k)
loadIndex parseKey indexDir =
  listDirectory indexDir >>= Monoid.foldMapM loadDependency
  where
  loadDependency :: FilePath -> m (Relation Reference k)
  loadDependency b@"_builtin" =
    listDirectory (indexDir </> b) >>= Monoid.foldMapM loadBuiltinDependency
    where
    loadBuiltinDependency :: FilePath -> m (Relation Reference k)
    loadBuiltinDependency path =
      loadDependentsOf
        (Reference.Builtin . Text.pack . decodeFileName $ path)
        (indexDir </> b </> path)
  -- part of the previous definition; not the `_builtin` directory, so try 
  -- parsing as a derived id reference
  loadDependency path = case componentIdFromString path of
    Nothing -> pure mempty
    Just r ->
      loadDependentsOf (Reference.DerivedId r) (indexDir </> path)

  -- load the range of the relation directory
  loadDependentsOf :: Reference -> FilePath -> m (Relation Reference k)
  loadDependentsOf r path =
    listDirectory path <&>
      Relation.fromList . fmap (r,) . catMaybes . fmap parseKey

-- Relation Dependency Dependent, e.g. [(Set a -> List a, Set.toList)]
loadTypeIndexDir :: MonadIO m => CodebasePath -> m (Relation Reference Referent.Id)
loadTypeIndexDir =
  loadIndex referentIdFromString . typeIndexDir'

-- Relation Dependency Dependent, e.g. [(Set, Set.toList), (List, Set.toList)]
loadTypeMentionsDir :: MonadIO m => CodebasePath -> m (Relation Reference Referent.Id)
loadTypeMentionsDir =
  loadIndex referentIdFromString . typeMentionsIndexDir'
