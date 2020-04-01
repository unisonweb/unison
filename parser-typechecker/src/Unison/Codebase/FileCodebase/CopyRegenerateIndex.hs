{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.FileCodebase.CopyRegenerateIndex (syncToDirectory) where

import Unison.Prelude

import qualified Data.Set                      as Set
import           UnliftIO.Directory             ( doesFileExist )
import           System.FilePath                ( FilePath )
import qualified Unison.Codebase               as Codebase
import qualified Unison.Codebase.Causal        as Causal
import           Unison.Codebase.Branch         ( Branch(Branch) )
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Codebase.Serialization.V1 as V1
import qualified Unison.DataDeclaration        as DD
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
import           Unison.Referent                ( pattern Ref
                                                , pattern Con
                                                )
import qualified Unison.Referent               as Referent
import qualified Unison.Term                   as Term
import           Unison.Type                    ( Type )
import qualified Unison.Type                   as Type
import           Unison.Var                     ( Var )
import qualified Unison.UnisonFile             as UF
import qualified Unison.Util.Star3             as Star3
import qualified Unison.Util.Relation          as Relation
import Control.Monad.State (StateT, MonadState)
import qualified Control.Monad.State           as State
import Control.Lens
import Unison.Util.Relation (Relation)
import Data.Monoid.Generic
import Unison.Codebase.FileCodebase.Common

data SyncedEntities = SyncedEntities
  { _syncedTerms       :: Set Reference.Id
  , _syncedDecls       :: Set Reference.Id
  , _syncedEdits       :: Set Branch.EditHash
  , _dependentsIndex   :: Relation Reference Reference.Id
  , _typeIndex         :: Relation Reference Referent.Id
  , _typeMentionsIndex :: Relation Reference Referent.Id
  } deriving Generic
  deriving Semigroup via GenericSemigroup SyncedEntities
  deriving Monoid via GenericMonoid SyncedEntities

makeLenses ''SyncedEntities

syncToDirectory :: forall m v a
  . MonadIO m
  => Var v
  => S.Get v
  -> S.Get a
  -> FilePath
  -> FilePath
  -> Branch m
  -> m (Branch m)
syncToDirectory getV getA srcPath destPath branch =
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
    writeDependentsIndex =<< use dependentsIndex
    writeTypeIndex =<< use typeIndex
    writeTypeMentionsIndex =<< use typeMentionsIndex
    updateCausalHead (branchHeadDir destPath) c
    pure branch
  where
  writeDependentsIndex
    :: Relation Reference Reference.Id -> StateT SyncedEntities m ()
  writeDependentsIndex =
    writeIndexHelper (\k v -> touchIdFile v (dependentsDir destPath k))
  writeTypeIndex, writeTypeMentionsIndex
    :: Relation Reference Referent.Id -> StateT SyncedEntities m ()
  writeTypeIndex =
    writeIndexHelper (\k v -> touchReferentIdFile v (typeIndexDir destPath k))
  writeTypeMentionsIndex =
    writeIndexHelper (\k v -> touchReferentIdFile v (typeMentionsIndexDir destPath k))
  writeIndexHelper
    :: forall m a b. MonadIO m => (a -> b -> m ()) -> Relation a b -> m ()
  writeIndexHelper touchIndexFile index =
    traverse_ @[] @m (uncurry touchIndexFile) (Relation.toList index)
  serialize :: Causal.Serialize (StateT SyncedEntities m) Branch.Raw Branch.Raw
  serialize rh rawBranch = unlessM (lift $ hashExists destPath rh) $ do
    writeBranch $ Causal.rawHead rawBranch
    lift $ serializeRawBranch destPath rh rawBranch
    where
    writeBranch :: Branch.Raw -> StateT SyncedEntities m ()
    writeBranch (Branch.Raw terms types _ _) = do
      -- Index and copy decls
      for_ (toList $ Star3.fact types) $ \case
        Reference.DerivedId i -> copyDecl i
        Reference.Builtin{} -> pure ()
      -- Index and copy term definitions
      for_ (toList $ Star3.fact terms) $ \case
        Ref (Reference.DerivedId i) -> copyTerm i
        Ref Reference.Builtin{} -> pure ()
        Con{} -> pure ()
    copyDecl :: Reference.Id -> StateT SyncedEntities m ()
    copyDecl = copyHelper destPath syncedDecls declPath $ \i -> do
      copyFileWithParents (declPath srcPath i) (declPath destPath i)
      liftIO (getDecl getV getA srcPath i) >>= \case
        Just decl -> do
          let referentTypes :: [(Referent.Id, Type v a)]
              referentTypes = DD.declConstructorReferents i decl
                              `zip` (DD.constructorTypes . DD.asDataDecl) decl
          for_ referentTypes $ \(r, typ) -> do
            dependentsIndex <>=
              Relation.fromManyDom (Type.dependencies typ) i
            typeIndex <>=
              Relation.singleton (Type.toReference typ) r
            typeMentionsIndex <>=
              Relation.fromManyDom (Type.toReferenceMentions typ) r
        Nothing ->
          fail $ "😞 The namespace " ++ show rh
               ++ " referenced the type " ++ show i
               ++ ", but I couldn't find it in " ++ (declPath srcPath i)
    copyTerm :: Reference.Id -> StateT SyncedEntities m ()
    copyTerm = copyHelper destPath syncedTerms termPath $
      \i -> do
        -- copy the files, and hopefully leave them in the disk cache for when
        -- we read them in a second!
        copyFileWithParents (termPath srcPath i) (termPath destPath i) -- compiled.ub
        copyFileWithParents (typePath srcPath i) (typePath destPath i) -- type.ub
        whenM (doesFileExist $ watchPath srcPath UF.TestWatch i) $
          copyFileWithParents (watchPath srcPath UF.TestWatch i)
                              (watchPath destPath UF.TestWatch i)
        -- build the dependents index from dependencies
        liftIO (S.getFromFile (V1.getTerm getV getA) (termPath srcPath i)) >>= \case
          Just term ->
            dependentsIndex <>= Relation.fromManyDom (Term.dependencies term) i
          Nothing ->
            fail $ "😞 The namespace " ++ show rh
                 ++ " referenced the term " ++ show i
                 ++ ", but I couldn't find it in " ++ (termPath srcPath i)
        -- build the type indices
        liftIO (S.getFromFile (V1.getType getV getA) (typePath srcPath i)) >>= \case
          Just typ -> do
            typeIndex <>=
              Relation.singleton (Type.toReference typ) (Referent.Ref' i)
            typeMentionsIndex <>=
              Relation.fromManyDom (Type.toReferenceMentions typ) (Referent.Ref' i)
          Nothing ->
            fail $ "😞 The namespace " ++ show rh
                 ++ " referenced the term " ++ show i
                 ++ ", but I couldn't find its type in " ++ (typePath srcPath i)
  copyEdits :: Branch.EditHash -> StateT SyncedEntities m ()
  copyEdits = copyHelper destPath syncedEdits editsPath $
    \h -> copyFileWithParents (editsPath srcPath h) (editsPath destPath h)
  copyHelper :: forall m s h. (MonadIO m, MonadState s m, Ord h)
             => CodebasePath
             -> SimpleLens s (Set h) -- lens to track if `h` is already handled
             -> (FilePath -> h -> FilePath) -- codebasepath -> hash -> filepath
             -> (h -> m ()) -- handle h
             -> h -> m ()
  copyHelper destPath l getFilename f h =
    unlessM (use (l . to (Set.member h))) $ do
      l %= Set.insert h
      ifM (doesFileExist (getFilename destPath h)) (f h) (pure ())
