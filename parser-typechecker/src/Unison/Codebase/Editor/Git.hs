{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.Git where

import           Control.Monad                  ( when
                                                , unless
                                                )
import           Control.Monad.Extra            ( whenM )
import           Control.Monad.Trans            ( lift )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                , ExceptT
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Shellmet                       ( ($?), ($|) )
import           System.Directory               ( getCurrentDirectory
                                                , setCurrentDirectory
                                                , doesDirectoryExist
                                                , findExecutable
                                                , removeDirectoryRecursive
                                                )
import           System.FilePath                ( (</>) )
import           Unison.Codebase.GitError
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase                ( Codebase
                                                , syncToDirectory
                                                )
import           Unison.Codebase.FileCodebase   ( getRootBranch
                                                , codebasePath
                                                )
import           Unison.Codebase.Branch         ( Branch
                                                , headHash
                                                )

-- Given a local path, a remote git repo url, and branch/commit hash,
-- pulls the HEAD of that remote repo into the local path.
pullFromGit
  :: MonadIO m
  => MonadError GitError m => FilePath -> Text -> Text -> m ()
pullFromGit localPath url treeish = do
  wd <- prepGitPull localPath url
  pull url treeish
  liftIO $ setCurrentDirectory wd

prepGitPull
  :: MonadIO m => MonadError GitError m => FilePath -> Text -> m FilePath
prepGitPull localPath uri = do
  checkForGit
  wd <- liftIO getCurrentDirectory
  liftIO . whenM (doesDirectoryExist localPath) $ removeDirectoryRecursive
    localPath
  clone uri localPath
  liftIO $ setCurrentDirectory localPath
  isGitDir <- liftIO checkGitDir
  unless isGitDir . throwError $ NoLocalRepoAt localPath
  pure wd

-- Shallow pull in preparation for a push
shallowPullFromGit
  :: MonadIO m
  => MonadError GitError m => FilePath -> Text -> Text -> m ()
shallowPullFromGit localPath url treeish = do
  wd <- prepGitPull localPath url
  unless (Text.null treeish) $
    "git" ["checkout", treeish]
      `onError` "git" ["checkout", "-b", treeish]
      `onError` throwError (CheckoutFailed treeish)
  liftIO $ setCurrentDirectory wd

-- Given a local path, a remote repo url, and branch/commit hash, pulls the
-- HEAD of that remote repo into the local path and attempts to load it as a
-- branch. Then merges the branch into the given codebase.
pullGitRootBranch
  :: MonadIO m
  => FilePath
  -> Codebase m v a
  -> Text
  -> Text
  -> ExceptT GitError m (Branch m)
pullGitRootBranch localPath codebase url treeish = do
  pullFromGit localPath url treeish
  branch <- lift $ getRootBranch (localPath </> codebasePath)
  lift $ Codebase.syncFromDirectory codebase (localPath </> codebasePath)
  lift $ Codebase.getBranchForHash codebase (headHash branch)

checkForGit :: MonadIO m => MonadError GitError m => m ()
checkForGit = do
  gitPath <- liftIO $ findExecutable "git"
  when (isNothing gitPath) $ throwError NoGit

checkGitDir :: IO Bool
checkGitDir = (const True <$> "git" ["rev-parse", "--git-dir"]) $? pure False

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

pull :: MonadError GitError m => MonadIO m => Text -> Text -> m ()
pull uri treeish = do
  "git" ["fetch", uri, treeish] `onError` throwError (NoRemoteRepoAt uri)
  liftIO $ "git" ["checkout", treeish]

-- Clone the given remote repo and commit to the given local path.
-- Then given a codebase and a branch, write the branch and all its
-- dependencies to the path, then commit and push to the remote repo.
pushGitRootBranch
  :: MonadIO m
  => FilePath
  -> Codebase m v a
  -> Branch m
  -> Text
  -> Text
  -> ExceptT GitError m ()
pushGitRootBranch localPath codebase branch url treeish = do
  wd <- liftIO getCurrentDirectory
  -- Clone and pull the remote repo
  shallowPullFromGit localPath url treeish
  -- Stick our changes in the checked-out copy
  lift $ syncToDirectory codebase (localPath </> codebasePath) branch
  liftIO $ do
    setCurrentDirectory localPath
    -- Commit our changes
    status <- "git" $| ["status", "--short"]
    unless (Text.null status) $ do
      "git" ["add", "--all", "."]
      "git" ["commit", "-m", "Sync branch " <> Text.pack (show $ headHash branch)]
    -- Push our changes to the repo
    if Text.null treeish
      then "git" ["push", "--all", url]
      else "git" ["push", url, treeish]
    setCurrentDirectory wd

