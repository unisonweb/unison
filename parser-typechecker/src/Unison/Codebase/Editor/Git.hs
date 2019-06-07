{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.Git where

import           Control.Monad                  ( when )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                , ExceptT
                                                )
import           Control.Monad.Trans.Except     ( mapExceptT )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Bifunctor                 ( first )
import           Data.Maybe                     ( isNothing )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Shellmet                       ( ($?) )
import           System.Directory               ( getCurrentDirectory
                                                , setCurrentDirectory
                                                , doesDirectoryExist
                                                , findExecutable
                                                )
import           Unison.Codebase.FileCodebase2  ( CodebasePath
                                                , getRootBranch
                                                , putRootBranch
                                                , Err
                                                )
import           Unison.Codebase.Branch2        ( Branch )

data GitError = NoGit
              | NoGithubAt Text
              | NotAGitRepo FilePath
              | Err Err

-- may need to be different for private repo?
pullFromGithub
  :: MonadIO m
  => MonadError GitError m => CodebasePath -> Text -> Text -> Text -> m ()
pullFromGithub localPath user repo treeish = do
  gitPath <- liftIO $ findExecutable "git"
  when (isNothing gitPath) $ throwError NoGit
  wd     <- liftIO $ getCurrentDirectory
  exists <- liftIO . doesDirectoryExist $ localPath
    --   3. The remote repo doesn't exist
    --   4. We don't have access to the repo
    --   6. The project is not a Unison repo
  when (not exists) $ shallowClone uri localPath
  liftIO . setCurrentDirectory $ localPath
  isGitDir <- liftIO checkGitDir
  when (not isGitDir) . throwError $ NotAGitRepo localPath
  shallowPull uri treeish
  liftIO $ setCurrentDirectory wd
  where uri = githubUri user repo

pullGithubRootBranch
  :: MonadIO m
  => CodebasePath
  -> Text
  -> Text
  -> Text
  -> ExceptT GitError m (Branch (ExceptT Err m))
pullGithubRootBranch localPath user repo treeish = do
  pullFromGithub localPath user repo treeish
  mapExceptT (fmap $ first Err) (getRootBranch localPath)

checkGitDir :: IO Bool
checkGitDir = (const True <$> "git" ["rev-parse", "--git-dir"]) $? pure False

githubUri :: Text -> Text -> Text
githubUri user repo = "git@github.com:" <> user <> "/" <> repo <> ".git"

onError :: MonadError e m => MonadIO m => IO () -> m () -> m ()
onError x k = liftIO ((const True <$> x) $? pure False) >>= \case
  True  -> pure ()
  False -> k

shallowClone :: MonadError GitError m => MonadIO m => Text -> FilePath -> m ()
shallowClone uri localPath =
  "git" ["clone", "--depth=1", uri, Text.pack localPath]
    `onError` throwError (NoGithubAt uri)

shallowPull :: MonadError GitError m => MonadIO m => Text -> Text -> m ()
shallowPull uri treeish = do
  "git" ["fetch", "--depth=1", uri, treeish]
    `onError` throwError (NoGithubAt uri)
  liftIO $ "git" ["checkout", treeish]

pushGithubRootBranch
  :: MonadError Err m
  => MonadIO m => CodebasePath -> Text -> Text -> Text -> Branch m -> m ()
pushGithubRootBranch localPath user repo ghbranch b = do
  -- Error conditions:
  --   1. The path is not a git repo
  --   2. The remote repo doesn't exist
  --   3. Git is not installed
  --   4. We don't have push rights to the remote
  --   5. The local path does not exist.
  putRootBranch localPath b
  liftIO $ do
  -- Write the branch to the local path
    wd <- getCurrentDirectory
    setCurrentDirectory localPath
    -- Commit our changes
    "git" ["commit", "-m", "Sync Unison Codebase"]
    -- Push our changes to the repo
    "git" ["push", "--all", githubUri user repo, ghbranch]
    setCurrentDirectory wd
