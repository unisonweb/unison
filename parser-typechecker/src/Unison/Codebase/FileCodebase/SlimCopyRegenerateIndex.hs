{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.FileCodebase.SlimCopyRegenerateIndex (syncToDirectory) where

import Control.Lens
import Control.Monad.State.Strict (MonadState, evalStateT)
import Control.Monad.Writer.Strict (MonadWriter, execWriterT)
import qualified Control.Monad.Writer.Strict as Writer
import Data.Monoid.Generic
import qualified Data.Set as Set
import Unison.Codebase (CodebasePath)
import Unison.Codebase.Branch (Branch (..))
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Branch.Dependencies as BD
import qualified Unison.Codebase.Causal as Causal
import Unison.Codebase.FileCodebase.Common
import qualified Unison.Codebase.Patch as Patch
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Codebase.Serialization.V1 as V1
import Unison.Codebase.SyncMode (SyncMode)
import qualified Unison.Codebase.SyncMode as SyncMode
import qualified Unison.Codebase.TermEdit as TermEdit
import qualified Unison.Codebase.TypeEdit as TypeEdit
import qualified Unison.DataDeclaration as DD
import qualified Unison.LabeledDependency as LD
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import qualified Unison.Term as Term
import Unison.Type (Type)
import qualified Unison.Type as Type
import qualified Unison.UnisonFile as UF
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as Relation
import Unison.Util.Timing (time)
import Unison.Var (Var)
import UnliftIO.Directory (doesFileExist)

data SyncedEntities = SyncedEntities
  { _syncedTerms :: Set Reference.Id,
    _syncedDecls :: Set Reference.Id,
    _syncedEdits :: Set Branch.EditHash,
    _syncedBranches :: Set Branch.Hash,
    _dependentsIndex :: Relation Reference Reference.Id,
    _typeIndex :: Relation Reference Referent.Id,
    _typeMentionsIndex :: Relation Reference Referent.Id
  }
  deriving (Generic)
  deriving (Show)
  deriving (Semigroup) via GenericSemigroup SyncedEntities
  deriving (Monoid) via GenericMonoid SyncedEntities

makeLenses ''SyncedEntities

syncToDirectory ::
  forall m v a.
  MonadIO m =>
  Var v =>
  S.Format v ->
  S.Format a ->
  CodebasePath ->
  CodebasePath ->
  SyncMode ->
  Branch m ->
  m ()
syncToDirectory fmtV fmtA = syncToDirectory' (S.get fmtV) (S.get fmtA)

data Error
  = MissingBranch Branch.Hash
  | MissingPatch Branch.EditHash
  | MissingTerm Reference.Id
  | MissingTypeOfTerm Reference.Id
  | MissingDecl Reference.Id
  | InvalidBranch Branch.Hash
  | InvalidTerm Reference.Id
  | InvalidTypeOfTerm Reference.Id
  | InvalidDecl Reference.Id
  deriving (Eq, Ord, Show)

syncToDirectory' ::
  forall m v a.
  MonadIO m =>
  Var v =>
  S.Get v ->
  S.Get a ->
  CodebasePath ->
  CodebasePath ->
  SyncMode ->
  Branch m ->
  m ()
syncToDirectory' getV getA srcPath destPath mode newRoot =
  let warnMissingEntities = False
   in flip evalStateT mempty $ do
        -- MonadState s m
        (deps, errors) <-
          time "Sync Branches" $
            execWriterT $
              processBranches
                [ ( Branch.headHash newRoot,
                    Just . pure . Branch.transform (lift . lift) $ newRoot
                  )
                ]
        errors' <-
          time "Sync Definitions" $
            execWriterT $ processDependencies (BD.to' deps)
        time "Write indices" $ do
          lift . writeDependentsIndex =<< use dependentsIndex
          lift . writeTypeIndex =<< use typeIndex
          lift . writeTypeMentionsIndex =<< use typeMentionsIndex
        when (warnMissingEntities) $ for_ (errors <> errors') traceShowM
  where
    writeDependentsIndex :: MonadIO m => Relation Reference Reference.Id -> m ()
    writeDependentsIndex = writeIndexHelper (\k v -> touchIdFile v (dependentsDir destPath k))
    writeTypeIndex, writeTypeMentionsIndex :: MonadIO m => Relation Reference Referent.Id -> m ()
    writeTypeIndex =
      writeIndexHelper (\k v -> touchReferentIdFile v (typeIndexDir destPath k))
    writeTypeMentionsIndex =
      writeIndexHelper (\k v -> touchReferentIdFile v (typeMentionsIndexDir destPath k))
    writeIndexHelper ::
      forall m a b. MonadIO m => (a -> b -> m ()) -> Relation a b -> m ()
    writeIndexHelper touchIndexFile index =
      traverse_ (uncurry touchIndexFile) (Relation.toList index)
    processBranches ::
      forall m.
      MonadIO m =>
      MonadState SyncedEntities m =>
      MonadWriter (BD.Dependencies, Set Error) m =>
      [(Branch.Hash, Maybe (m (Branch m)))] ->
      m ()
    processBranches [] = pure ()
    -- for each branch,
    processBranches ((h, mmb) : rest) =
      let tellError = Writer.tell . (mempty,) . Set.singleton
          tellDependencies = Writer.tell . (,mempty)
       in -- if hash exists at the destination, skip it, mark it done
          ifNeedsSyncing
            h
            destPath
            branchPath
            syncedBranches
            ( \h ->
                -- else if hash exists at the source, enqueue its dependencies, copy it, mark it done
                ifM
                  (doesFileExist (branchPath srcPath h))
                  ( do
                      (branches, deps) <-
                        BD.fromRawCausal
                          <$> (deserializeRawBranchDependencies tellError srcPath h)
                      copyFileWithParents (branchPath srcPath h) (branchPath destPath h)
                      tellDependencies deps
                      processBranches (branches ++ rest)
                  )
                  -- else if it's in memory, enqueue its dependencies, write it, mark it done
                  case mmb of
                    Just mb -> do
                      b <- mb
                      let (branches, deps) = BD.fromBranch b
                      let causalRaw = Branch.toCausalRaw b
                      serializeRawBranch destPath h causalRaw
                      tellDependencies deps
                      processBranches (branches ++ rest)
                    -- else -- error?
                    Nothing -> do
                      tellError (MissingBranch h)
                      processBranches rest
            )
            (processBranches rest)
    processDependencies ::
      forall n.
      MonadIO n =>
      MonadState SyncedEntities n =>
      MonadWriter (Set Error) n =>
      BD.Dependencies' ->
      n ()
    processDependencies = \case
      -- for each patch
      -- enqueue its target term and type references
      BD.Dependencies' (editHash : editHashes) terms decls ->
        -- This code assumes that patches are always available on disk,
        -- not ever just held in memory with `pure`.  If that's not the case,
        -- then we can do something similar to what we did  with branches.
        ifNeedsSyncing
          editHash
          destPath
          editsPath
          syncedEdits
          ( \h -> do
              patch <- deserializeEdits srcPath h
              -- I'm calling all the replacement terms dependents of the patches.
              -- If we're supposed to replace X with Y, we don't necessarily need X,
              -- but we do need Y.
              let newTerms, newDecls :: [Reference.Id]
                  newTerms =
                    [ i
                      | TermEdit.Replace (Reference.DerivedId i) _ <-
                          toList . Relation.ran $ Patch._termEdits patch
                    ]
                  newDecls =
                    [ i
                      | TypeEdit.Replace (Reference.DerivedId i) <-
                          toList . Relation.ran $ Patch._typeEdits patch
                    ]
              ifM
                (doesFileExist (editsPath srcPath h))
                ( do
                    copyFileWithParents (editsPath srcPath h) (editsPath destPath h)
                    processDependencies $
                      BD.Dependencies' editHashes (newTerms ++ terms) (newDecls ++ decls)
                )
                ( do
                    tellError (MissingPatch h)
                    (processDependencies $ BD.Dependencies' editHashes terms decls)
                )
          )
          (processDependencies $ BD.Dependencies' editHashes terms decls)
      -- for each term id
      BD.Dependencies' [] (termHash : termHashes) decls ->
        -- if it exists at the destination, skip it, mark it done
        ifNeedsSyncing
          termHash
          destPath
          termPath
          syncedTerms
          ( \h -> do
              -- else if it exists at the source,
              ifM
                (doesFileExist (termPath srcPath h))
                ( do
                    -- copy it,
                    -- load it,
                    -- enqueue its dependencies for syncing
                    -- enqueue its type's type dependencies for syncing
                    -- enqueue its type's dependencies, type & type mentions into respective indices
                    -- and continue
                    (newTerms, newDecls) <- enqueueTermDependencies h
                    processDependencies $
                      BD.Dependencies' [] (newTerms ++ termHashes) (newDecls ++ decls)
                )
                -- else -- an error?
                ( do
                    tellError (MissingTerm h)
                    (processDependencies $ BD.Dependencies' [] termHashes decls)
                )
          )
          (processDependencies $ BD.Dependencies' [] termHashes decls)
      -- for each decl id
      BD.Dependencies' [] [] (declHash : declHashes) ->
        -- if it exists at the destination, skip it, mark it done
        ifNeedsSyncing
          declHash
          destPath
          declPath
          syncedDecls
          ( \h -> do
              -- else if it exists at the source,
              ifM
                (doesFileExist (declPath srcPath h))
                -- copy it,
                -- load it,
                -- enqueue its type dependencies for syncing
                -- for each constructor,
                -- enqueue its dependencies, type, type mentions into respective indices
                ( do
                    newDecls <- copyAndIndexDecls h
                    processDependencies $ BD.Dependencies' [] [] (newDecls ++ declHashes)
                )
                ( do
                    tellError (MissingDecl h)
                    (processDependencies $ BD.Dependencies' [] [] declHashes)
                )
          )
          (processDependencies $ BD.Dependencies' [] [] declHashes)
      BD.Dependencies' [] [] [] -> pure ()
    copyAndIndexDecls ::
      forall m.
      MonadIO m =>
      MonadState SyncedEntities m =>
      MonadWriter (Set Error) m =>
      Reference.Id ->
      m [Reference.Id]
    copyAndIndexDecls h =
      (getDecl getV getA srcPath h :: m (Maybe (DD.Decl v a))) >>= \case
        Just decl -> do
          copyFileWithParents (declPath srcPath h) (declPath destPath h)
          let referentTypes :: [(Referent.Id, Type v a)]
              referentTypes =
                DD.declConstructorReferents h decl
                  `zip` (DD.constructorTypes . DD.asDataDecl) decl
          flip foldMapM referentTypes \(r, typ) -> do
            let dependencies = toList (Type.dependencies typ)
            dependentsIndex <>= Relation.fromManyDom dependencies h
            let typeForIndexing = Type.removeAllEffectVars typ
            let typeReference = Type.toReference typeForIndexing
            let typeMentions = Type.toReferenceMentions typeForIndexing
            typeIndex <>= Relation.singleton typeReference r
            typeMentionsIndex <>= Relation.fromManyDom typeMentions r
            pure [i | Reference.DerivedId i <- dependencies]
        Nothing -> tellError (InvalidDecl h) $> mempty

    enqueueTermDependencies ::
      forall m.
      MonadIO m =>
      MonadState SyncedEntities m =>
      MonadWriter (Set Error) m =>
      Reference.Id ->
      m ([Reference.Id], [Reference.Id])
    enqueueTermDependencies h =
      getTerm getV getA srcPath h >>= \case
        Just term -> do
          let (typeDeps, termDeps) =
                partitionEithers . fmap LD.toReference . toList $
                  Term.labeledDependencies term
          ifM
            (doesFileExist (typePath srcPath h))
            ( getTypeOfTerm getV getA srcPath h >>= \case
                Just typ -> do
                  copyFileWithParents (termPath srcPath h) (termPath destPath h)
                  copyFileWithParents (typePath srcPath h) (typePath destPath h)
                  whenM (doesFileExist $ watchPath srcPath UF.TestWatch h) $
                    copyFileWithParents
                      (watchPath srcPath UF.TestWatch h)
                      (watchPath destPath UF.TestWatch h)
                  let typeDeps' = toList (Type.dependencies typ)
                  let typeForIndexing = Type.removeAllEffectVars typ
                  let typeReference = Type.toReference typeForIndexing
                  let typeMentions = Type.toReferenceMentions typeForIndexing
                  dependentsIndex
                    <>= Relation.fromManyDom (typeDeps ++ typeDeps' ++ termDeps) h
                  typeIndex
                    <>= Relation.singleton typeReference (Referent.Ref' h)
                  typeMentionsIndex
                    <>= Relation.fromManyDom typeMentions (Referent.Ref' h)
                  let newDecls = [i | Reference.DerivedId i <- typeDeps ++ typeDeps']
                  let newTerms = [i | Reference.DerivedId i <- termDeps]
                  pure (newTerms, newDecls)
                Nothing -> tellError (InvalidTypeOfTerm h) $> mempty
            )
            (tellError (MissingTypeOfTerm h) $> mempty)
        Nothing -> tellError (InvalidTerm h) $> mempty

    deserializeRawBranchDependencies ::
      forall m.
      MonadIO m =>
      (Error -> m ()) ->
      CodebasePath ->
      Causal.Deserialize m Branch.Raw (BD.Branches m, BD.Dependencies)
    deserializeRawBranchDependencies tellError root h =
      S.getFromFile (V1.getCausal0 V1.getBranchDependencies) (branchPath root h) >>= \case
        Nothing -> tellError (InvalidBranch h) $> Causal.RawOne mempty
        Just results -> pure results
    tellError :: forall m a. MonadWriter (Set a) m => a -> m ()
    tellError = Writer.tell . Set.singleton

    -- Use State and Lens to do some specified thing at most once, to create a file.
    ifNeedsSyncing ::
      forall m s h.
      (MonadIO m, MonadState s m, Ord h) =>
      h ->
      CodebasePath ->
      (CodebasePath -> h -> FilePath) -> -- done if this filepath exists
      SimpleLens s (Set h) -> -- lens to track if `h` is already done
      (h -> m ()) -> -- do!
      m () -> -- don't
      m ()
    ifNeedsSyncing h destPath getFilename l doSync dontSync =
      ifM (use (l . to (Set.member h))) dontSync $ do
        l %= Set.insert h
        if mode == SyncMode.Complete
          then doSync h
          else ifM (doesFileExist (getFilename destPath h)) dontSync (doSync h)
