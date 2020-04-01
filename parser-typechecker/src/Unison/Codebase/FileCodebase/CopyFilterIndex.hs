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

import qualified Data.Set                      as Set
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
import           Unison.Referent                ( Referent
                                                , pattern Ref
                                                , pattern Con
                                                )
import qualified Unison.Referent               as Referent
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Star3             as Star3
import qualified Unison.Util.Relation          as Relation
import Control.Monad.State (StateT)
import qualified Control.Monad.State           as State
import Control.Lens
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Monoid as Monoid
import Data.Monoid.Generic-- (GenericSemigroup, GenericMonoid)
import Unison.Codebase.FileCodebase.Common
import Data.Set (Set)
import GHC.Generics (Generic)
import Unison.Prelude (MonadIO)

data SyncedEntities = SyncedEntities
  { _syncedTerms    :: Set Reference.Id
  , _syncedDecls    :: Set Reference.Id
  , _syncedReferents :: Set Referent
  , _syncedEdits    :: Set Branch.EditHash
  } deriving Generic
  deriving Semigroup via GenericSemigroup SyncedEntities
  deriving Monoid via GenericMonoid SyncedEntities

makeLenses ''SyncedEntities

syncToDirectory :: forall m
  . MonadIO m
  => FilePath
  -> FilePath
  -> Branch m
  -> m (Branch m)
syncToDirectory srcPath destPath branch =
  (`State.evalStateT` mempty) $ do
    b <- (liftIO . exists) destPath
    newRemoteRoot@(Branch c) <- lift $
      if b then
        -- we are merging the specified branch with the destination root;
        -- alternatives would be to replace the root, or leave it untouched,
        -- meaning this new data could be garbage-colleged
        getRootBranch destPath >>= \case
          -- todo: `fail` isn't great.
          Left Codebase.NoRootBranch ->
            fail $ "No root branch found in " ++ destPath
          Left (Codebase.CouldntLoadRootBranch h) ->
            fail $ "Couldn't find root branch " ++ show h ++ " in " ++ destPath
          Right existingDestRoot ->
            Branch.merge branch existingDestRoot
      else pure branch
    Branch.sync
      (hashExists destPath)
      serialize
      (\h _me -> copyEdits h)
      (Branch.transform lift newRemoteRoot)
    x <- use syncedTerms
    y <- use syncedDecls
    copyDependents x y
    z <- use syncedReferents
    copyTypeIndex z
    copyTypeMentionsIndex z
    updateCausalHead (branchHeadDir destPath) c
    pure branch
  where
  -- the terms and types we copied, we should transfer info about their dependencies
  copyDependents :: forall m. MonadIO m => Set Reference.Id -> Set Reference.Id -> m ()
  copyDependents terms types =
    copyIndexHelper
      loadDependentsDir
      (\k v -> touchIdFile v (dependentsDir destPath k))
      (terms <> types)
  copyTypeIndex :: forall m. MonadIO m => Set Referent -> m ()
  copyTypeIndex =
    copyIndexHelper
      loadTypeIndexDir
      (\k v -> touchReferentFile v (typeIndexDir destPath k))
  copyTypeMentionsIndex :: forall m. MonadIO m => Set Referent -> m ()
  copyTypeMentionsIndex =
    copyIndexHelper
      loadTypeMentionsDir
      (\k v -> touchReferentFile v (typeMentionsIndexDir destPath k))
  copyIndexHelper :: forall m d r. MonadIO m
                  => (Ord d, Ord r)
                  => (CodebasePath -> m (Relation d r))
                  -> (d -> r -> m ())
                  -> Set r
                  -> m ()
  copyIndexHelper loadIndex touchIndexFile neededSet = do
    available <- loadIndex srcPath
    let needed = Relation.restrictRan available neededSet
    traverse_ @[] @m (uncurry touchIndexFile) (Relation.toList needed)

  serialize :: Causal.Serialize (StateT SyncedEntities m) Branch.Raw Branch.Raw
  serialize rh rawBranch = unlessM (lift $ hashExists destPath rh) $ do
    writeBranch $ Causal.rawHead rawBranch
    lift $ serializeRawBranch destPath rh rawBranch
    where
    writeBranch :: Branch.Raw -> StateT SyncedEntities m ()
    writeBranch (Branch.Raw terms types _ _) = do
      -- Copy decls and enqueue Ids for dependents indexing
      for_ (toList $ Star3.fact types) $ \case
        Reference.DerivedId i -> copyDecl i
        Reference.Builtin{} -> pure ()
      -- Copy term definitions,
      -- enqueue term `Reference.Id`s for dependents indexing,
      -- and enqueue all referents for indexing
      for_ (toList $ Star3.fact terms) $ \r -> do
        case r of
          Ref (Reference.DerivedId i) -> copyTerm i
          Ref Reference.Builtin{} -> pure ()
          Con{} -> pure ()
        syncedReferents %= Set.insert r
    copyDecl :: Reference.Id -> StateT SyncedEntities m ()
    copyDecl = copyHelper destPath syncedDecls declPath $
      \i -> copyFileWithParents (declPath srcPath i) (declPath destPath i)
    copyTerm :: Reference.Id -> StateT SyncedEntities m ()
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
loadDependentsDir ::
  MonadIO m => CodebasePath -> m (Relation Reference Reference.Id)
loadDependentsDir =
  loadIndex (Reference.idFromText . Text.pack) . dependentsDir'

-- todo: delete Show constraint
loadIndex :: forall m k. (MonadIO m, Ord k) => Show k
           => (String -> Maybe k) -> FilePath -> m (Relation Reference k)
loadIndex parseKey indexDir =
  listDirectory indexDir >>= Monoid.foldMapM loadDependency
  where
  loadDependency :: FilePath -> m (Relation Reference k)
  loadDependency b@"_builtin" = do
    listDirectory (indexDir </> b) >>= Monoid.foldMapM loadBuiltinDependency
    where
    loadBuiltinDependency :: FilePath -> m (Relation Reference k)
    loadBuiltinDependency path =
      loadDependentsOf
        (Reference.Builtin (Text.pack path))
        (indexDir </> b </> path)

  loadDependency path = case componentIdFromString path of
    Nothing -> pure mempty
    Just r ->
      loadDependentsOf (Reference.DerivedId r) (indexDir </> path)

  loadDependentsOf :: Reference -> FilePath -> m (Relation Reference k)
  loadDependentsOf r path = do
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

