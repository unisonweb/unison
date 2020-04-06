{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.FileCodebase.CopyFilterIndex (syncToDirectory) where

import Unison.Prelude

import qualified Data.Text                     as Text
import           UnliftIO.Directory             ( doesFileExist
                                                , listDirectory
                                                )
import           System.FilePath                ( FilePath
                                                , (</>)
                                                )
import qualified Unison.Codebase               as Codebase
import qualified Unison.Codebase.Causal        as Causal
import           Unison.Codebase.Branch         ( Branch(Branch) )
import qualified Unison.Codebase.Branch        as Branch
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import           Unison.Referent                ( Referent )
import qualified Unison.Referent               as Referent
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Star3             as Star3
import qualified Unison.Util.Relation          as Relation
import Control.Monad.State (StateT)
import qualified Control.Monad.State           as State
import Control.Lens
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Monoid as Monoid
import Data.Monoid.Generic
import Unison.Codebase.FileCodebase.Common
import Data.Set (Set)
import GHC.Generics (Generic)
import Unison.Prelude (MonadIO)
import qualified Unison.Util.Set as Set
import Unison.Util.TransitiveClosure as TC

data SyncedEntities = SyncedEntities
  { _syncedTerms     :: Set Reference.Id
  , _syncedDecls     :: Set Reference.Id
  , _typeIndexQueue  :: Set Referent
  , _syncedEdits     :: Set Branch.EditHash
  } deriving Generic
  deriving Semigroup via GenericSemigroup SyncedEntities
  deriving Monoid via GenericMonoid SyncedEntities

makeLenses ''SyncedEntities

-- Create a codebase structure at `destPath` if none exists, and
-- copy (merge) all codebase elements from the current codebase into it.
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
syncToDirectory :: forall m
  . MonadIO m
  => FilePath
  -> FilePath
  -> Branch m
  -> m (Branch m)
syncToDirectory srcPath destPath branch =
  flip State.evalStateT mempty $ do
    newRemoteRoot@(Branch c) <- lift $
      ifM (exists destPath)
        (getRootBranch destPath >>= \case
          Right existingDestRoot -> Branch.merge branch existingDestRoot
          -- The destination codebase doesn't advertise a root branch,
          -- so we'll just use ours.
          Left Codebase.NoRootBranch -> pure branch
          Left (Codebase.CouldntLoadRootBranch h) -> fail $
            "I was trying to merge with the existing root branch at " ++
            branchPath destPath h ++ ", but the file was missing."
          )
        -- else there was no existing codebase structure at `destPath` so whatev
        (pure branch)
    Branch.sync
      (hashExists destPath)
      copyRawBranch
      (\h _me -> copyEdits h)
      (Branch.transform lift newRemoteRoot)
    x <- use syncedTerms
    y <- use syncedDecls
    dependentsIndex <- loadDependentsDir srcPath
    copyTransitiveDependencies (x <> y) dependentsIndex
    x <- use syncedTerms
    y <- use syncedDecls
    copyDependentsIndex (x <> y) dependentsIndex
    q <- use typeIndexQueue
    copyTypeIndex q
    copyTypeMentionsIndex q
    updateCausalHead (branchHeadDir destPath) c
    pure branch
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

  copyTypeIndex, copyTypeMentionsIndex :: forall m. MonadIO m => Set Referent -> m ()
  copyTypeIndex = copyIndexHelper loadTypeIndexDir $
    \k v -> touchReferentFile v (typeIndexDir destPath k)
  copyTypeMentionsIndex = copyIndexHelper loadTypeMentionsDir $
    \k v -> touchReferentFile v (typeMentionsIndexDir destPath k)

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
      -- and enqueue all referents for indexing
      traverse_ copyTerm
        . mapMaybe Reference.toId
        . mapMaybe Referent.toTermReference
        . toList
        $ Star3.fact terms
      typeIndexQueue <>= Star3.fact terms
  copyDecl, copyTerm :: Reference.Id -> StateT SyncedEntities m ()
  copyDecl = copyHelper destPath syncedDecls declPath $
    \i -> copyFileWithParents (declPath srcPath i) (declPath destPath i)
  copyTerm = copyHelper destPath syncedTerms termPath $
    \i -> do
      copyFileWithParents (termPath srcPath i) (termPath destPath i) -- compiled.ub
      copyFileWithParents (typePath srcPath i) (typePath destPath i) -- type.ub
      whenM (doesFileExist $ watchPath srcPath UF.TestWatch i) $
        copyFileWithParents (watchPath srcPath UF.TestWatch i)
                            (watchPath destPath UF.TestWatch i)
  copyEdits :: Branch.EditHash -> StateT SyncedEntities m ()
  copyEdits = copyHelper destPath syncedEdits editsPath $
    \h -> copyFileWithParents (editsPath srcPath h) (editsPath destPath h)

-- Relation Dependency Dependent, e.g. [(List.foldLeft, List.reverse)]
-- root / "dependents" / "_builtin" / Nat / yourFunction
loadDependentsDir :: MonadIO m => CodebasePath -> m (Relation Reference Reference.Id)
loadDependentsDir = loadIndex (Reference.idFromText . Text.pack) . dependentsDir'

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

  loadDependency path = case componentIdFromString path of
    Nothing -> pure mempty
    Just r ->
      loadDependentsOf (Reference.DerivedId r) (indexDir </> path)

  loadDependentsOf :: Reference -> FilePath -> m (Relation Reference k)
  loadDependentsOf r path =
    listDirectory path <&>
      Relation.fromList . fmap (r,) . catMaybes . fmap parseKey

-- Relation Dependency Dependent, e.g. [(Set a -> List a, Set.toList)]
loadTypeIndexDir :: MonadIO m => CodebasePath -> m (Relation Reference Referent)
loadTypeIndexDir =
  loadIndex (Referent.fromText . Text.pack) . typeIndexDir'

-- Relation Dependency Dependent, e.g. [(Set, Set.toList), (List, Set.toList)]
loadTypeMentionsDir :: MonadIO m => CodebasePath -> m (Relation Reference Referent)
loadTypeMentionsDir =
  loadIndex (Referent.fromText . Text.pack) . typeMentionsIndexDir'

