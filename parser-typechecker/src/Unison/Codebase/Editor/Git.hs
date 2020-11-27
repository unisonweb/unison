{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Editor.Git
  ( importRemoteBranch,
    pushGitRootBranch,
    viewRemoteBranch,
  )
where

import qualified Control.Exception
import Control.Monad.Except
  ( ExceptT,
    MonadError,
    throwError,
  )
import Control.Monad.Extra ((||^))
import qualified Data.Text as Text
import Shellmet (($?), ($^), ($|))
import System.FilePath ((</>))
import Unison.Codebase (Codebase, CodebasePath)
import qualified Unison.Codebase as Codebase
import Unison.Codebase.Branch
  ( Branch,
    headHash,
  )
import qualified Unison.Codebase.Branch as Branch
import Unison.Codebase.Editor.RemoteRepo
  ( RemoteNamespace,
    RemoteRepo (GitRepo),
    printRepo,
  )
import Unison.Codebase.FileCodebase as FC
import Unison.Codebase.FileCodebase.Common (branchHeadDir, encodeFileName, updateCausalHead)
import Unison.Codebase.GitError (GitError)
import qualified Unison.Codebase.GitError as GitError
import qualified Unison.Codebase.Path as Path
import Unison.Codebase.SyncMode (SyncMode)
import Unison.Prelude
import qualified Unison.Util.Exception as Ex
import Unison.Util.Timing (time)
import UnliftIO.Directory (XdgDirectory (XdgCache), doesDirectoryExist, findExecutable, getXdgDirectory, removeDirectoryRecursive)
import UnliftIO.IO (hFlush, stdout)

tempGitDir :: MonadIO m => Text -> m FilePath
tempGitDir url =
  getXdgDirectory XdgCache $
    "unisonlanguage"
      </> "gitfiles"
      </> encodeFileName (Text.unpack url)

withStatus :: MonadIO m => String -> m a -> m a
withStatus str ma = do
  flushStr str
  a <- ma
  flushStr (const ' ' <$> str)
  pure a
  where
    flushStr str = do
      liftIO . putStr $ "  " ++ str ++ "\r"
      hFlush stdout

-- | Given a remote git repo url, and branch/commit hash (currently
-- not allowed): checks for git, clones or updates a cached copy of the repo
pullBranch :: (MonadIO m, MonadError GitError m) => RemoteRepo -> m CodebasePath
pullBranch (GitRepo _uri (Just t)) =
  error $
    "Pulling a specific commit isn't fully implemented or tested yet.\n"
      ++ "InputPatterns.parseUri was expected to have prevented you "
      ++ "from supplying the git treeish `"
      ++ Text.unpack t
      ++ "`!"
pullBranch repo@(GitRepo uri Nothing) = do
  checkForGit
  localPath <- tempGitDir uri
  ifM
    (doesDirectoryExist localPath)
    -- try to update existing directory
    ( ifM
        (isGitRepo localPath)
        (checkoutExisting localPath)
        (throwError (GitError.UnrecognizableCacheDir uri localPath))
    )
    -- directory doesn't exist, so clone anew
    (checkOutNew localPath Nothing)
  pure localPath
  where
    checkOutNew :: (MonadIO m, MonadError GitError m) => CodebasePath -> Maybe Text -> m ()
    checkOutNew localPath branch = do
      withStatus ("Downloading from " ++ Text.unpack uri ++ " ...") $
        ( liftIO $
            "git"
              $^ ( ["clone", "--quiet"] ++ ["--depth", "1"]
                     ++ maybe [] (\t -> ["--branch", t]) branch
                     ++ [uri, Text.pack localPath]
                 )
        )
          `withIOError` (throwError . GitError.CloneException repo . show)
      isGitDir <- liftIO $ isGitRepo localPath
      unless isGitDir . throwError $ GitError.UnrecognizableCheckoutDir uri localPath
    checkoutExisting :: (MonadIO m, MonadError GitError m) => FilePath -> m ()
    checkoutExisting localPath =
      ifM
        (isEmptyGitRepo localPath)
        -- I don't know how to properly update from an empty remote repo.
        -- As a heuristic, if this cached copy is empty, then the remote might
        -- be too, so this impl. just wipes the cached copy and starts from scratch.
        (do wipeDir localPath; checkOutNew localPath Nothing)
        -- Otherwise proceed!
        ( withStatus ("Updating cached copy of " ++ Text.unpack uri ++ " ...") $ do
            gitIn localPath ["reset", "--hard", "--quiet", "HEAD"]
            gitIn localPath ["clean", "-d", "--force", "--quiet"]
            gitIn localPath ["pull", "--force", "--quiet"]
        )

    isEmptyGitRepo :: MonadIO m => FilePath -> m Bool
    isEmptyGitRepo localPath =
      liftIO $
        -- if rev-parse succeeds, the repo is _not_ empty, so return False; else True
        (gitTextIn localPath ["rev-parse", "--verify", "--quiet", "HEAD"] $> False)
          $? pure True
    wipeDir localPath = do
      e <-
        Ex.tryAny . whenM (doesDirectoryExist localPath) $
          removeDirectoryRecursive localPath
      case e of
        Left e -> throwError (GitError.SomeOtherError (show e))
        Right _ -> pure ()

-- | Sync elements as needed from a remote codebase into the local one.
-- If `sbh` is supplied, we try to load the specified branch hash;
-- otherwise we try to load the root branch.
importRemoteBranch ::
  forall m v a.
  MonadIO m =>
  Codebase m v a ->
  Branch.Cache m ->
  RemoteNamespace ->
  SyncMode ->
  ExceptT GitError m (Branch m)
importRemoteBranch codebase cache ns mode = do
  (branch, cacheDir) <- viewRemoteBranch' cache ns
  withStatus "Importing downloaded files into local codebase..." $
    time "SyncFromDirectory" $
      lift $ Codebase.syncFromDirectory codebase cacheDir mode branch
  pure branch

-- | Pull a git branch and view it from the cache, without syncing into the
-- local codebase.
viewRemoteBranch ::
  forall m.
  MonadIO m =>
  Branch.Cache m ->
  RemoteNamespace ->
  ExceptT GitError m (Branch m)
viewRemoteBranch cache = fmap fst . viewRemoteBranch' cache

viewRemoteBranch' ::
  forall m.
  MonadIO m =>
  Branch.Cache m ->
  RemoteNamespace ->
  ExceptT GitError m (Branch m, CodebasePath)
viewRemoteBranch' cache (repo, sbh, path) = do
  -- set up the cache dir
  remotePath <- time "Git fetch" $ pullBranch repo
  -- try to load the requested branch from it
  branch <- time "Git fetch (sbh)" $ case sbh of
    -- load the root branch
    Nothing ->
      lift (FC.getRootBranch cache remotePath) >>= \case
        Left Codebase.NoRootBranch -> pure Branch.empty
        Left (Codebase.CouldntLoadRootBranch h) ->
          throwError $ GitError.CouldntLoadRootBranch repo h
        Left (Codebase.CouldntParseRootBranch s) ->
          throwError $ GitError.CouldntParseRootBranch repo s
        Right b -> pure b
    -- load from a specific `ShortBranchHash`
    Just sbh -> do
      branchCompletions <- lift $ FC.branchHashesByPrefix remotePath sbh
      case toList branchCompletions of
        [] -> throwError $ GitError.NoRemoteNamespaceWithHash repo sbh
        [h] ->
          (lift $ FC.branchFromFiles cache remotePath h) >>= \case
            Just b -> pure b
            Nothing -> throwError $ GitError.NoRemoteNamespaceWithHash repo sbh
        _ -> throwError $ GitError.RemoteNamespaceHashAmbiguous repo sbh branchCompletions
  pure (Branch.getAt' path branch, remotePath)

-- | See if `git` is on the system path.
checkForGit :: MonadIO m => MonadError GitError m => m ()
checkForGit = do
  gitPath <- liftIO $ findExecutable "git"
  when (isNothing gitPath) $ throwError GitError.NoGit

-- | Does `git` recognize this directory as being managed by git?
isGitRepo :: MonadIO m => FilePath -> m Bool
isGitRepo dir =
  liftIO $
    (True <$ gitIn dir ["rev-parse"]) $? pure False

-- | Perform an IO action, passing any IO exception to `handler`
withIOError :: MonadIO m => IO a -> (IOException -> m a) -> m a
withIOError action handler =
  liftIO (fmap Right action `Control.Exception.catch` (pure . Left))
    >>= either handler pure

-- | Generate some `git` flags for operating on some arbitary checked out copy
setupGitDir :: FilePath -> [Text]
setupGitDir localPath =
  [ "--git-dir",
    Text.pack $ localPath </> ".git",
    "--work-tree",
    Text.pack localPath
  ]

gitIn :: MonadIO m => FilePath -> [Text] -> m ()
gitIn localPath args = liftIO $ "git" $^ (setupGitDir localPath <> args)

gitTextIn :: MonadIO m => FilePath -> [Text] -> m Text
gitTextIn localPath args = liftIO $ "git" $| setupGitDir localPath <> args

-- Given a branch that is "after" the existing root of a given git repo,
-- stage and push the branch (as the new root) + dependencies to the repo.
pushGitRootBranch ::
  MonadIO m =>
  Codebase m v a ->
  Branch.Cache m ->
  Branch m ->
  RemoteRepo ->
  SyncMode ->
  ExceptT GitError m ()
pushGitRootBranch codebase cache branch repo syncMode = do
  -- Pull the remote repo into a staging directory
  (remoteRoot, remotePath) <- viewRemoteBranch' cache (repo, Nothing, Path.empty)
  ifM
    ( pure (remoteRoot == Branch.empty)
        ||^ lift (remoteRoot `Branch.before` branch)
    )
    -- ours is newer 👍, meaning this is a fast-forward push,
    -- so sync branch to staging area
    (stageAndPush remotePath)
    (throwError $ GitError.PushDestinationHasNewStuff repo)
  where
    stageAndPush remotePath = do
      let repoString = Text.unpack $ printRepo repo
      withStatus ("Staging files for upload to " ++ repoString ++ " ...") $
        lift (Codebase.syncToDirectory codebase remotePath syncMode branch)
      updateCausalHead (branchHeadDir remotePath) (Branch._history branch)
      -- push staging area to remote
      withStatus ("Uploading to " ++ repoString ++ " ...") $
        unlessM
          ( push remotePath repo
              `withIOError` (throwError . GitError.PushException repo . show)
          )
          (throwError $ GitError.PushNoOp repo)
    -- Commit our changes
    push :: CodebasePath -> RemoteRepo -> IO Bool -- withIOError needs IO
    push remotePath (GitRepo url gitbranch) = do
      -- has anything changed?
      status <- gitTextIn remotePath ["status", "--short"]
      if Text.null status
        then pure False
        else do
          gitIn remotePath ["add", "--all", "."]
          gitIn
            remotePath
            ["commit", "-q", "-m", "Sync branch " <> Text.pack (show $ headHash branch)]
          -- Push our changes to the repo
          case gitbranch of
            Nothing -> gitIn remotePath ["push", "--quiet", url]
            Just gitbranch ->
              error $
                "Pushing to a specific branch isn't fully implemented or tested yet.\n"
                  ++ "InputPatterns.parseUri was expected to have prevented you "
                  ++ "from supplying the git treeish `"
                  ++ Text.unpack gitbranch
                  ++ "`!"
          -- gitIn remotePath ["push", "--quiet", url, gitbranch]
          pure True
