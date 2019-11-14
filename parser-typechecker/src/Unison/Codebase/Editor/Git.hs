{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.Git where

import Unison.Prelude

import           Control.Monad.Catch            ( MonadCatch
                                                , onException
                                                )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                , ExceptT
                                                )
import qualified Data.Text                     as Text
import           Shellmet                       ( ($?), ($|) )
import           System.Directory               ( doesDirectoryExist
                                                , findExecutable
                                                , removeDirectoryRecursive
                                                )
import           System.FilePath                ( (</>) )
import           Unison.Codebase.GitError
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase                ( Codebase
                                                , syncToDirectory
                                                )
import           Unison.Codebase.FileCodebase  as FC
import           Unison.Codebase.Branch         ( Branch
                                                , headHash
                                                )
import qualified Unison.Util.Exception         as Ex
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Names3                 as Names
import Unison.Codebase.ShortBranchHash (ShortBranchHash)

-- Given a local path, a remote git repo url, and branch/commit hash,
-- pulls the HEAD of that remote repo into the local path.
pullFromGit
  :: MonadIO m
  => MonadError GitError m => FilePath -> Text -> Maybe Text -> m ()
pullFromGit localPath url treeish = do
  prepGitPull localPath url
  pull localPath url treeish

-- checks for git, the repo path, performs a clone, and verifies resulting repo
prepGitPull
  :: MonadIO m => MonadError GitError m => FilePath -> Text -> m ()
prepGitPull localPath uri = do
  checkForGit
  e <- liftIO . Ex.tryAny . whenM (doesDirectoryExist localPath) $
    removeDirectoryRecursive localPath
  case e of
    Left e -> throwError (SomeOtherError (Text.pack (show e)))
    Right _ -> pure ()
  clone uri localPath
  isGitDir <- liftIO $ checkGitDir localPath
  unless isGitDir . throwError $ NoLocalRepoAt localPath

-- Shallow pull in preparation for a push
shallowPullFromGit
  :: MonadIO m
  => MonadError GitError m => FilePath -> Text -> Maybe Text -> m ()
shallowPullFromGit localPath url gitBranch = do
  prepGitPull localPath url
  for_ gitBranch $ \gitBranch ->
    gitIn localPath ["checkout", gitBranch]
      -- creates a new branch in prep for pushing,
      -- not sure what happens if the branch has the same name as a commit 😬
      `onError` gitIn localPath ["checkout", "-b", gitBranch]
      `onError` throwError (CheckoutFailed gitBranch)

-- Given a local path, a remote repo url, and branch/commit hash, pulls the
-- HEAD of that remote repo into the local path and attempts to load it as a
-- branch. Then merges the branch into the given codebase.
pullGitRootBranch
  :: MonadIO m
  => FilePath
  -> Codebase m v a
  -> Text
  -> Maybe Text
  -> ExceptT GitError m (Branch m)
pullGitRootBranch localPath codebase url treeish =
  pullGitBranch localPath codebase url treeish Nothing

-- pull repo & load arbitrary branch
pullGitBranch
  :: MonadIO m
  => FilePath
  -> Codebase m v a
  -> Text
  -> Maybe Text
  -> Maybe ShortBranchHash
  -> ExceptT GitError m (Branch m)
pullGitBranch localPath codebase url treeish sbh = do
  pullFromGit localPath url treeish
  branch <- case sbh of
    Nothing -> lift $ FC.getRootBranch (localPath </> codebasePath)
    Just sbh -> do
      branchCompletions <- lift $ FC.branchHashesByPrefix gitCodebasePath sbh
      case toList branchCompletions of
        [] -> throwError $ NoRemoteNamespaceWithHash url treeish sbh
        [h] -> lift $ FC.branchFromFiles FC.FailIfMissing gitCodebasePath h
        _ -> throwError $ RemoteNamespaceHashAmbiguous url treeish sbh branchCompletions
  lift $ Codebase.syncFromDirectory codebase gitCodebasePath
  pure branch
  where gitCodebasePath = localPath </> codebasePath

checkForGit :: MonadIO m => MonadError GitError m => m ()
checkForGit = do
  gitPath <- liftIO $ findExecutable "git"
  when (isNothing gitPath) $ throwError NoGit

checkGitDir :: FilePath -> IO Bool
checkGitDir dir =
  (const True <$> gitIn dir ["rev-parse", "--git-dir"]) $? pure False

onError :: MonadError e m => MonadIO m => IO () -> m () -> m ()
onError x k = liftIO ((const True <$> x) $? pure False) >>= \case
  True  -> pure ()
  False -> k

clone :: MonadError GitError m => MonadIO m => Text -> FilePath -> m ()
clone uri localPath = "git" ["clone", uri, Text.pack localPath]
  `onError` throwError (NoRemoteRepoAt uri)

shallowClone :: MonadError GitError m => MonadIO m => Text -> FilePath -> m ()
shallowClone uri localPath =
  "git" ["clone", "--depth=1", uri, Text.pack localPath]
    `onError` throwError (NoRemoteRepoAt uri)

pull :: MonadError GitError m => MonadIO m => FilePath -> Text -> Maybe Text -> m ()
pull localPath uri treeish = do
  gitIn localPath (["fetch", uri] ++ toList treeish)
    `onError` throwError (NoRemoteRepoAt uri)
  for_ treeish $ \treeish ->
    liftIO $ gitIn localPath ["checkout", treeish]

gitIn :: FilePath -> [Text] -> IO ()
gitIn localPath args = "git" (["-C", Text.pack localPath] <> args)

gitTextIn :: FilePath -> [Text] -> IO Text
gitTextIn localPath args = "git" $| ["-C", Text.pack localPath] <> args

-- Clone the given remote repo and commit to the given local path.
-- Then given a codebase and a branch, write the branch and all its
-- dependencies to the path, then commit and push to the remote repo.
pushGitRootBranch
  :: MonadIO m
  => MonadCatch m
  => FilePath
  -> Codebase m v a
  -> Branch m
  -> Text
  -> Maybe Text
  -> ExceptT GitError m ()
pushGitRootBranch localPath codebase branch url gitbranch = do
  -- Clone and pull the remote repo
  shallowPullFromGit localPath url gitbranch
  -- Stick our changes in the checked-out copy
  merged <- lift $ syncToDirectory codebase (localPath </> codebasePath) branch
  isBefore <- lift $ Branch.before merged branch
  let mergednames = Branch.toNames0 (Branch.head merged)
      localnames  = Branch.toNames0 (Branch.head branch)
      diff = Names.diff0 localnames mergednames
  when (not isBefore) $
    throwError (PushDestinationHasNewStuff url gitbranch diff)
  let
    push = do
      -- Commit our changes
      status <- gitTextIn localPath ["status", "--short"]
      unless (Text.null status) $ do
        gitIn localPath ["add", "--all", "."]
        gitIn localPath
          ["commit", "-m", "Sync branch " <> Text.pack (show $ headHash branch)]
      -- Push our changes to the repo
      case gitbranch of
        Nothing        -> gitIn localPath ["push", "--all", url]
        Just gitbranch -> gitIn localPath ["push", url, gitbranch]
  liftIO push `onException` throwError (NoRemoteRepoAt url)

