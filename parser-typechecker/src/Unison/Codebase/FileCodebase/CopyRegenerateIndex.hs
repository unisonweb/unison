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

import           UnliftIO.Directory             ( doesFileExist )
import           Unison.Codebase                ( CodebasePath )
import qualified Unison.Codebase.Causal        as Causal
import           Unison.Codebase.Branch         ( Branch(Branch) )
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Codebase.Serialization as S
import qualified Unison.DataDeclaration        as DD
import           Unison.Reference               ( Reference )
import qualified Unison.Reference              as Reference
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
import Unison.Codebase.Patch (Patch)

data SyncedEntities = SyncedEntities
  { _syncedTerms       :: Set Reference.Id
  , _syncedDecls       :: Set Reference.Id
  , _syncedEdits       :: Set Branch.EditHash
  , _dependentsIndex   :: Relation Reference Reference.Id
  , _typeIndex         :: Relation Reference Referent.Id
  , _typeMentionsIndex :: Relation Reference Referent.Id
  } deriving Generic
  deriving Show
  deriving Semigroup via GenericSemigroup SyncedEntities
  deriving Monoid via GenericMonoid SyncedEntities

makeLenses ''SyncedEntities

syncToDirectory :: forall m v a
  . MonadIO m
  => Var v
  => S.Format v
  -> S.Format a
  -> CodebasePath
  -> CodebasePath
  -> Branch m
  -> m ()
syncToDirectory fmtV fmtA = syncToDirectory' (S.get fmtV) (S.get fmtA)

-- Copy (merge) all dependent codebase elements of `branch` from `srcPath` into 
-- `destPath`, and set `branch` as the new root in `destPath`.
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
-- branch from memory if it doesn't exist on disk; e.g. a merge root node.
--
-- Our `serializeEdits` ignores the `m Patch` it's given, and assumes it can
-- file-copy one from disk based on the provided EditsHash.
--
-- Although branches, terms, decls, and edits are copied directly from source
-- to destination without going through the serialization process, the
-- dependents, type, and type mention indices are populated by loading the source
-- definitions, and then creating the appropriate files in the destination index
--
-- Finally, this implementation does write all the transitive dependencies and
-- indices by loading each term as it's copied, and recursing over each
-- dependency.  It uses SyncedEntities to track what's already been done and not
-- repeat work.  It does deserialize definitions to find their dependencies, but
-- it doesn't do any serialization, except in the situation where we are trying
-- to copy Branch history that only exists in memory and not in the FileCodebase.
syncToDirectory' :: forall m v a
  . MonadIO m
  => Var v
  => S.Get v
  -> S.Get a
  -> CodebasePath
  -> CodebasePath
  -> Branch m
  -> m ()
syncToDirectory' getV getA srcPath destPath newRemoteRoot@(Branch c) =
  flip State.evalStateT mempty $ do
    Branch.sync
      (hashExists destPath)
      copyRawBranch
      (flip copyEdits)
      (Branch.transform lift newRemoteRoot)
    writeDependentsIndex =<< use dependentsIndex
    writeTypeIndex =<< use typeIndex
    writeTypeMentionsIndex =<< use typeMentionsIndex
    updateCausalHead (branchHeadDir destPath) c
  where
  writeDependentsIndex :: Relation Reference Reference.Id -> StateT SyncedEntities m ()
  writeDependentsIndex = writeIndexHelper (\k v -> touchIdFile v (dependentsDir destPath k))
  writeTypeIndex, writeTypeMentionsIndex
    :: Relation Reference Referent.Id -> StateT SyncedEntities m ()
  writeTypeIndex =
    writeIndexHelper (\k v -> touchReferentIdFile v (typeIndexDir destPath k))
  writeTypeMentionsIndex =
    writeIndexHelper (\k v -> touchReferentIdFile v (typeMentionsIndexDir destPath k))
  writeIndexHelper
    :: forall m a b. MonadIO m => (a -> b -> m ()) -> Relation a b -> m ()
  writeIndexHelper touchIndexFile index =
    traverse_ (uncurry touchIndexFile) (Relation.toList index)
  copyRawBranch :: (MonadState SyncedEntities n, MonadIO n) 
                => Branch.Hash -> Causal.Raw Branch.Raw Branch.Raw -> n ()
  copyRawBranch rh rc = unlessM (hashExists destPath rh) $ do
    copyReferencedDefns $ Causal.rawHead rc
    copyOrReserializeRawBranch rh rc
    where
    copyOrReserializeRawBranch rh rc =
      ifM (doesFileExist (branchPath srcPath rh))
        (copyFileWithParents (branchPath srcPath rh) (branchPath destPath rh))
        (serializeRawBranch destPath rh rc)
    copyReferencedDefns :: (MonadState SyncedEntities n, MonadIO n) => Branch.Raw -> n ()
    copyReferencedDefns (Branch.Raw terms types _ _) = do
      -- Index and copy decls
      traverse_ copyDecl
        . mapMaybe Reference.toId
        . toList
        $ Star3.fact types
      -- Index and copy term definitions
      traverse_ copyTerm
        . mapMaybe Reference.toId
        . mapMaybe Referent.toTermReference
        . toList
        $ Star3.fact terms
  -- copy and index a decl and its transitive dependencies
  copyDecl :: (MonadState SyncedEntities n, MonadIO n) => Reference.Id -> n ()
  copyDecl = doFileOnce destPath syncedDecls declPath $ \i -> do
    -- first copy the file, then recursively copy its dependencies
    copyFileWithParents (declPath srcPath i) (declPath destPath i)
    getDecl getV getA srcPath i >>= \case
      Just decl -> do
        let referentTypes :: [(Referent.Id, Type v a)]
            referentTypes = DD.declConstructorReferents i decl
                            `zip` (DD.constructorTypes . DD.asDataDecl) decl
        for_ referentTypes $ \(r, typ) -> do
          let dependencies = Type.dependencies typ
          traverse copyDecl . mapMaybe Reference.toId $ toList dependencies
          dependentsIndex <>=
            Relation.fromManyDom dependencies i
          typeIndex <>=
            Relation.singleton (Type.toReference typ) r
          typeMentionsIndex <>=
            Relation.fromManyDom (Type.toReferenceMentions typ) r
      Nothing ->
        fail $ "😞 I encountered a reference to the type " ++ show i
             ++ ", but I couldn't find it in " ++ declPath srcPath i
  -- copy and index a term and its transitive dependencies
  copyTerm :: MonadState SyncedEntities n => MonadIO n => Reference.Id -> n ()
  copyTerm = doFileOnce destPath syncedTerms termPath $
    \i -> do
      -- copy the files, and hopefully leave them in the disk cache for when
      -- we read them in a second!
      copyFileWithParents (termPath srcPath i) (termPath destPath i) -- compiled.ub
      copyFileWithParents (typePath srcPath i) (typePath destPath i) -- type.ub
      whenM (doesFileExist $ watchPath srcPath UF.TestWatch i) $
        copyFileWithParents (watchPath srcPath UF.TestWatch i)
                            (watchPath destPath UF.TestWatch i)
      -- build up the dependents index from dependencies,
      -- and recursively copy the dependencies too.
      getTerm getV getA srcPath i >>= \case
        Just term -> do
          let dependencies = Term.dependencies term
          dependentsIndex <>= Relation.fromManyDom dependencies i
          -- copy the transitive dependencies
          for_ (mapMaybe Reference.toId $ toList dependencies) $ \r ->
            ifM (isTerm r) (copyTerm r) $
              ifM (isDecl r) (copyDecl r) $
                liftIO . putStrLn $
                  "❗️ I was trying to copy the definition of " ++ show r ++
                  ", which is a dependency of " ++ show i ++
                  ", but I couldn't find it as a type _or_ a term."
            where
            isTerm = doesFileExist . termPath srcPath
            isDecl = doesFileExist . declPath srcPath
        Nothing ->
          fail $ "😞 I was trying to copy the type " ++ show i
               ++ ", but I couldn't find it in " ++ termPath srcPath i
      -- build the type indices
      getTypeOfTerm getV getA srcPath i >>= \case
        Just typ -> do
          let dependencies = Type.dependencies typ
          let typeForIndexing = Type.removeAllEffectVars typ
          let typeReference = Type.toReference typeForIndexing
          let typeMentions = Type.toReferenceMentions typeForIndexing
          dependentsIndex <>= Relation.fromManyDom dependencies i
          typeIndex <>=
            Relation.singleton typeReference (Referent.Ref' i)
          typeMentionsIndex <>=
            Relation.fromManyDom typeMentions (Referent.Ref' i)
        Nothing ->
          fail $ "😞 I was trying to copy the term " ++ show i
               ++ ", but I couldn't find its type in " ++ typePath srcPath i
  copyEdits :: StateT SyncedEntities m Patch -> Branch.EditHash -> StateT SyncedEntities m ()
  copyEdits p = doFileOnce destPath syncedEdits editsPath $ \h -> do
    ifM (doesFileExist (editsPath srcPath h))
        (copyFileWithParents (editsPath srcPath h) (editsPath destPath h))
        (serializeEdits destPath h p)
