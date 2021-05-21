{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.FileCodebase
  (
    codebase1', -- used by Test/Git
    Unison.Codebase.FileCodebase.init,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Exception.Safe (MonadCatch, catchIO)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import System.Directory (canonicalizePath)
import System.FilePath (dropExtension)
import Unison.Codebase (BuiltinAnnotation, Codebase (Codebase), CodebasePath)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch (Branch)
import qualified Unison.Codebase.Branch as Branch
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Extra ((||^))
import System.FilePath ((</>))
import qualified U.Util.Cache as Cache
import qualified Unison.Codebase.Init as Codebase
import Unison.Codebase.Branch (headHash)
import Unison.Codebase.Editor.Git (gitIn, gitTextIn, pullBranch, withIOError, withStatus)
import Unison.Codebase.Editor.RemoteRepo (RemoteNamespace, RemoteRepo (GitRepo), printRepo)
import Unison.Codebase.FileCodebase.Common
  ( Err (CantParseBranchHead),
    branchFromFiles,
    branchHashesByPrefix,
    branchHeadDir,
    codebaseExists,
    componentIdFromString,
    decodeFileName,
    dependentsDir,
    failWith,
    formatAnn,
    getDecl,
    getPatch,
    getRootBranch,
    getTerm,
    getTypeOfTerm,
    getWatch,
    hashExists,
    hashFromFilePath,
    listDirectory,
    patchExists,
    putBranch,
    putDecl,
    putRootBranch,
    putTerm,
    putWatch,
    referentIdFromString,
    reflogPath,
    serializeEdits,
    termReferencesByPrefix,
    termReferentsByPrefix,
    typeIndexDir,
    typeMentionsIndexDir,
    typeReferencesByPrefix,
    updateCausalHead,
    watchesDir,
  )
import qualified Unison.Codebase.FileCodebase.Common as Common
import qualified Unison.Codebase.FileCodebase.SlimCopyRegenerateIndex as Sync
import Unison.Codebase.GitError (GitError)
import qualified Unison.Codebase.GitError as GitError
import qualified Unison.Codebase.Path as Path
import qualified Unison.Codebase.Reflog as Reflog
import qualified Unison.Codebase.Serialization as S
import qualified Unison.Codebase.Serialization.V1 as V1
import Unison.Codebase.SyncMode (SyncMode)
import qualified Unison.Codebase.Watch as Watch
import Unison.Parser (Ann ())
import Unison.Prelude
import Unison.Reference (Reference)
import qualified Unison.Reference as Reference
import qualified Unison.Referent as Referent
import Unison.Symbol (Symbol)
import qualified Unison.UnisonFile as UF
import qualified Unison.Util.Pretty as P
import qualified Unison.Util.TQueue as TQueue
import U.Util.Timing (time)
import Unison.Var (Var)
import UnliftIO.Directory (createDirectoryIfMissing, doesDirectoryExist)
import UnliftIO.STM (atomically)

init :: (MonadIO m, MonadCatch m) => Codebase.Init m Symbol Ann
init = Codebase.Init
  ((fmap . fmap) (pure (),) . openCodebase)
  ((fmap . fmap) (pure (),) . createCodebase)
  (</> Common.codebasePath)


-- get the codebase in dir
openCodebase :: forall m. (MonadIO m, MonadCatch m) => CodebasePath -> m (Either Codebase.Pretty (Codebase m Symbol Ann))
openCodebase dir = do
  prettyDir <- liftIO $ P.string <$> canonicalizePath dir
  let theCodebase = codebase1 @m @Symbol @Ann Cache.nullCache V1.formatSymbol formatAnn dir
  ifM (codebaseExists dir)
    (Right <$> theCodebase)
    (pure . Left $ "No FileCodebase structure found at " <> prettyDir)

createCodebase ::
  forall m.
  (MonadIO m, MonadCatch m) =>
  CodebasePath ->
  m (Either Codebase.CreateCodebaseError (Codebase m Symbol Ann))
createCodebase dir = ifM
  (codebaseExists dir)
  (pure $ Left Codebase.CreateCodebaseAlreadyExists)
  (do
    codebase <- codebase1 @m @Symbol @Ann Cache.nullCache V1.formatSymbol formatAnn dir
    Codebase.putRootBranch codebase Branch.empty
    pure $ Right codebase)

-- builds a `Codebase IO v a`, given serializers for `v` and `a`
codebase1
  :: forall m v a
   . MonadIO m
  => MonadCatch m
  => Var v
  => BuiltinAnnotation a
  => Branch.Cache m -> S.Format v -> S.Format a -> CodebasePath -> m (Codebase m v a)
codebase1 = codebase1' Sync.syncToDirectory

codebase1'
  :: forall m v a
   . MonadIO m
  => MonadCatch m
  => Var v
  => BuiltinAnnotation a
  => Common.SyncToDir m v a -> Branch.Cache m -> S.Format v -> S.Format a -> CodebasePath -> m (Codebase m v a)
codebase1' syncToDirectory branchCache fmtV@(S.Format getV putV) fmtA@(S.Format getA putA) path = do
  termCache <- Cache.semispaceCache 8192
  typeOfTermCache <- Cache.semispaceCache 8192
  declCache <- Cache.semispaceCache 1024
  let addDummyCleanup (a,b) = (pure (), a, b)
      c =
        Codebase
          (Cache.applyDefined termCache $ getTerm getV getA path)
          (Cache.applyDefined typeOfTermCache $ getTypeOfTerm getV getA path)
          (Cache.applyDefined declCache $ getDecl getV getA path)
          (putTerm putV putA path)
          (putDecl putV putA path)
          (getRootBranch branchCache path)
          (putRootBranch path)
          (branchHeadUpdates path)
          (branchFromFiles branchCache path)
          (putBranch path)
          (hashExists path)
          (getPatch path)
          (\h p -> serializeEdits path h (pure p))
          (patchExists path)
          dependents
          (flip (syncToDirectory fmtV fmtA) path)
          (syncToDirectory fmtV fmtA path)
          (runExceptT . fmap addDummyCleanup . viewRemoteBranch' Cache.nullCache)
          (\b r m -> runExceptT $
            pushGitRootBranch (syncToDirectory fmtV fmtA path) Cache.nullCache b r m)
          watches
          (getWatch getV getA path)
          (putWatch putV putA path)
          getReflog
          appendReflog
          getTermsOfType
          getTermsMentioningType
   -- todo: maintain a trie of references to come up with this number
          (pure 10)
   -- The same trie can be used to make this lookup fast:
          (termReferencesByPrefix path)
          (typeReferencesByPrefix path)
          (termReferentsByPrefix (getDecl getV getA) path)
          (pure 10)
          (branchHashesByPrefix path)
          Nothing -- just use in memory Branch.lca
          Nothing -- just use in memory Branch.before
   in pure c
  where
    dependents :: Reference -> m (Set Reference.Id)
    dependents r = listDirAsIds (dependentsDir path r)
    getTermsOfType :: Reference -> m (Set Referent.Id)
    getTermsOfType r = listDirAsReferents (typeIndexDir path r)
    getTermsMentioningType :: Reference -> m (Set Referent.Id)
    getTermsMentioningType r = listDirAsReferents (typeMentionsIndexDir path r)
  -- todo: revisit these
    listDirAsIds :: FilePath -> m (Set Reference.Id)
    listDirAsIds d = do
      e <- doesDirectoryExist d
      if e
        then do
          ls <- fmap decodeFileName <$> listDirectory d
          pure . Set.fromList $ ls >>= (toList . componentIdFromString)
        else pure Set.empty
    listDirAsReferents :: FilePath -> m (Set Referent.Id)
    listDirAsReferents d = do
      e <- doesDirectoryExist d
      if e
        then do
          ls <- fmap decodeFileName <$> listDirectory d
          pure . Set.fromList $ ls >>= (toList . referentIdFromString)
        else pure Set.empty
    watches :: UF.WatchKind -> m [Reference.Id]
    watches k =
      liftIO $ do
        let wp = watchesDir path (Text.pack k)
        createDirectoryIfMissing True wp
        ls <- listDirectory wp
        pure $ ls >>= (toList . componentIdFromString . dropExtension)
    getReflog :: m [Reflog.Entry]
    getReflog =
      liftIO
        (do contents <- TextIO.readFile (reflogPath path)
            let lines = Text.lines contents
            let entries = parseEntry <$> lines
            pure entries) `catchIO` const (pure [])
      where
        parseEntry t = fromMaybe (err t) (Reflog.fromText t)
        err t = error $
          "I couldn't understand this line in " ++ reflogPath path ++ "\n\n" ++
          Text.unpack t
    appendReflog :: Text -> Branch m -> Branch m -> m ()
    appendReflog reason old new =
      let
        t = Reflog.toText $
          Reflog.Entry (Branch.headHash old) (Branch.headHash new) reason
      in liftIO $ TextIO.appendFile (reflogPath path) (t <> "\n")

-- watches in `branchHeadDir root` for externally deposited heads;
-- parse them, and return them
branchHeadUpdates
  :: MonadIO m => CodebasePath -> m (IO (), IO (Set Branch.Hash))
branchHeadUpdates root = do
  branchHeadChanges      <- TQueue.newIO
  (cancelWatch, watcher) <- Watch.watchDirectory' (branchHeadDir root)
--  -- add .ubf file changes to intermediate queue
  watcher1               <-
    liftIO . forkIO
    $ forever
    $ do
      -- Q: what does watcher return on a file deletion?
      -- A: nothing
        (filePath, _) <- watcher
        case hashFromFilePath filePath of
          Nothing -> failWith $ CantParseBranchHead filePath
          Just h ->
            atomically . TQueue.enqueue branchHeadChanges $ Branch.Hash h
  -- smooth out intermediate queue
  pure
    ( cancelWatch >> killThread watcher1
    , Set.fromList <$> Watch.collectUntilPause branchHeadChanges 400000
    )

-- * Git stuff

viewRemoteBranch' :: forall m. MonadIO m
  => Branch.Cache m -> RemoteNamespace -> ExceptT GitError m (Branch m, CodebasePath)
viewRemoteBranch' cache (repo, sbh, path) = do
  -- set up the cache dir
  remotePath <- time "Git fetch" $ pullBranch repo
  -- try to load the requested branch from it
  branch <- time "Git fetch (sbh)" $ case sbh of
    -- load the root branch
    Nothing -> lift (getRootBranch cache remotePath) >>= \case
      Left Codebase.NoRootBranch -> pure Branch.empty
      Left (Codebase.CouldntLoadRootBranch h) ->
        throwError $ GitError.CouldntLoadRootBranch repo h
      Left (Codebase.CouldntParseRootBranch s) ->
        throwError $ GitError.CouldntParseRootBranch repo s
      Right b -> pure b
    -- load from a specific `ShortBranchHash`
    Just sbh -> do
      branchCompletions <- lift $ branchHashesByPrefix remotePath sbh
      case toList branchCompletions of
        [] -> throwError $ GitError.NoRemoteNamespaceWithHash repo sbh
        [h] -> (lift $ branchFromFiles cache remotePath h) >>= \case
          Just b -> pure b
          Nothing -> throwError $ GitError.NoRemoteNamespaceWithHash repo sbh
        _ -> throwError $ GitError.RemoteNamespaceHashAmbiguous repo sbh branchCompletions
  pure (Branch.getAt' path branch, remotePath)

-- Given a branch that is "after" the existing root of a given git repo,
-- stage and push the branch (as the new root) + dependencies to the repo.
pushGitRootBranch
  :: MonadIO m
  => Codebase.SyncToDir m
  -> Branch.Cache m
  -> Branch m
  -> RemoteRepo
  -> SyncMode
  -> ExceptT GitError m ()
pushGitRootBranch syncToDirectory cache branch repo syncMode = do
  -- Pull the remote repo into a staging directory
  (remoteRoot, remotePath) <- viewRemoteBranch' cache (repo, Nothing, Path.empty)
  ifM (pure (remoteRoot == Branch.empty)
        ||^ lift (remoteRoot `Branch.before` branch))
    -- ours is newer 👍, meaning this is a fast-forward push,
    -- so sync branch to staging area
    (stageAndPush remotePath)
    (throwError $ GitError.PushDestinationHasNewStuff repo)
  where
  stageAndPush remotePath = do
    let repoString = Text.unpack $ printRepo repo
    withStatus ("Staging files for upload to " ++ repoString ++ " ...") $
      lift (syncToDirectory remotePath syncMode branch)
    updateCausalHead (branchHeadDir remotePath) (Branch._history branch)
    -- push staging area to remote
    withStatus ("Uploading to " ++ repoString ++ " ...") $
      unlessM
        (push remotePath repo
          `withIOError` (throwError . GitError.PushException repo . show))
        (throwError $ GitError.PushNoOp repo)
  -- Commit our changes
  push :: CodebasePath -> RemoteRepo -> IO Bool -- withIOError needs IO
  push remotePath (GitRepo url gitbranch) = do
    -- has anything changed?
    status <- gitTextIn remotePath ["status", "--short"]
    if Text.null status then
      pure False
    else do
      gitIn remotePath ["add", "--all", "."]
      gitIn remotePath
        ["commit", "-q", "-m", "Sync branch " <> Text.pack (show $ headHash branch)]
      -- Push our changes to the repo
      case gitbranch of
        Nothing        -> gitIn remotePath ["push", "--quiet", url]
        Just gitbranch -> error $
          "Pushing to a specific branch isn't fully implemented or tested yet.\n"
          ++ "InputPatterns.parseUri was expected to have prevented you "
          ++ "from supplying the git treeish `" ++ Text.unpack gitbranch ++ "`!"
          -- gitIn remotePath ["push", "--quiet", url, gitbranch]
      pure True
