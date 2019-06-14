{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.Git where

import Debug.Trace
import           Control.Monad                  ( when
                                                , unless
                                                )
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
                                                , removeDirectory
                                                )
import           Unison.Codebase.GitError
import qualified Unison.Codebase2              as Codebase
import           Unison.Codebase2               ( Codebase
                                                , syncToDirectory
                                                )
import           Unison.Codebase.FileCodebase2  ( getRootBranch
                                                , branchHeadDir
                                                )
import           Unison.Codebase.Branch2        ( Branch
                                                , headHash
                                                )

-- Given a local path, a Github org, repo, and branch/commit hash,
-- pulls the HEAD of that Github location into the local path.
pullFromGithub
  :: MonadIO m
  => MonadError GitError m => FilePath -> Text -> Text -> Text -> m ()
pullFromGithub localPath user repo treeish = do
  wd <- prepGitPull localPath uri
  pull uri treeish
  liftIO $ setCurrentDirectory wd
  where uri = githubUri user repo

prepGitPull
  :: MonadIO m => MonadError GitError m => FilePath -> Text -> m FilePath
prepGitPull localPath uri = do
  checkForGit
  wd     <- liftIO getCurrentDirectory
  exists <- liftIO . doesDirectoryExist $ localPath
  unless exists $ clone uri localPath
  liftIO $ setCurrentDirectory localPath
  isGitDir <- liftIO checkGitDir
  unless isGitDir . throwError $ NotAGitRepo localPath
  pure wd

-- Shallow pull in preparation for a push
shallowPullFromGithub
  :: MonadIO m
  => MonadError GitError m => FilePath -> Text -> Text -> Text -> m ()
shallowPullFromGithub localPath user repo treeish = do
  wd <- prepGitPull localPath uri
  unless (Text.null treeish) $
    "git" ["checkout", treeish]
      `onError` "git" ["checkout", "-b", treeish]
      `onError` throwError (CheckoutFailed treeish)
  liftIO $ setCurrentDirectory wd
  where uri = githubUri user repo

-- Given a local path, a Github org, repo, and branch/commit hash, pulls the
-- HEAD of that Github location into the local path and attempts to load it as a
-- branch. Then merges the branch into the given codebase.
pullGithubRootBranch
  :: MonadIO m
  => FilePath
  -> Codebase m v a
  -> Text
  -> Text
  -> Text
  -> ExceptT GitError m (Branch m)
pullGithubRootBranch localPath codebase user repo treeish = do
  pullFromGithub localPath user repo treeish
  branch <- lift $ getRootBranch localPath
  headExists <- liftIO $ doesDirectoryExist $ branchHeadDir localPath
  when headExists $
    liftIO . removeDirectory $ branchHeadDir localPath
  lift $ Codebase.syncFromDirectory codebase localPath
  pure branch

checkForGit :: MonadIO m => MonadError GitError m => m ()
checkForGit = do
  gitPath <- liftIO $ findExecutable "git"
  when (isNothing gitPath) $ throwError NoGit

checkGitDir :: IO Bool
checkGitDir = (const True <$> "git" ["rev-parse", "--git-dir"]) $? pure False

githubUri :: Text -> Text -> Text
githubUri user repo = "git@github.com:" <> user <> "/" <> repo <> ".git"

onError :: MonadError e m => MonadIO m => IO () -> m () -> m ()
onError x k = liftIO ((const True <$> x) $? pure False) >>= \case
  True  -> pure ()
  False -> k

clone :: MonadError GitError m => MonadIO m => Text -> FilePath -> m ()
clone uri localPath = "git" ["clone", uri, Text.pack localPath]
  `onError` throwError (NoGithubAt uri)

shallowClone :: MonadError GitError m => MonadIO m => Text -> FilePath -> m ()
shallowClone uri localPath =
  "git" ["clone", "--depth=1", uri, Text.pack localPath]
    `onError` throwError (NoGithubAt uri)

pull :: MonadError GitError m => MonadIO m => Text -> Text -> m ()
pull uri treeish = do
  "git" ["fetch", uri, treeish] `onError` throwError (NoGithubAt uri)
  liftIO $ "git" ["checkout", treeish]

-- Clone the given github repo and commit to the given a local path.
-- Then given a codebase and a branch, write the branch and all its
-- dependencies to the path, then commit and push to github.
pushGithubRootBranch
  :: MonadIO m
  => FilePath
  -> Codebase m v a
  -> Branch m
  -> Text
  -> Text
  -> Text
  -> ExceptT GitError m ()
pushGithubRootBranch localPath codebase branch user repo treeish = do
  traceM "Pushing..."
  wd <- liftIO getCurrentDirectory
  -- Clone and pull the remote repo
  traceM "Shallow pull..."
  shallowPullFromGithub localPath user repo treeish
  -- Stick our changes in the checked-out copy
  traceM $ "Syncing code to " <> show localPath
  lift $ syncToDirectory codebase localPath branch
  liftIO $ do
    setCurrentDirectory localPath
    -- Commit our changes
    status <- "git" $| ["status", "--short"]
    unless (Text.null status) $ do
      "git" ["add", "--all", "."]
      "git" ["commit", "-m", "Sync branch " <> Text.pack (show $ headHash branch)]
    -- Push our changes to the repo
    if Text.null treeish
      then "git" ["push", "--all", githubUri user repo]
      else "git" ["push", githubUri user repo, treeish]
    setCurrentDirectory wd

