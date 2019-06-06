{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.Git where

import           Control.Monad                  ( when )
import           Control.Monad.Except           ( MonadError )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Shellmet                       ( )
import           System.Directory               ( getCurrentDirectory
                                                , setCurrentDirectory
                                                , doesDirectoryExist
                                                )
import           Unison.Codebase.FileCodebase2  ( CodebasePath
                                                , getRootBranch
                                                , putRootBranch
                                                , Err
                                                )
import           Unison.Codebase.Branch2        ( Branch )

-- may need to be different for private repo?
-- TODO: handle errors properly
pullGithubRootBranch
  :: MonadIO m
  => MonadError Err m => CodebasePath -> Text -> Text -> Text -> m (Branch m)
pullGithubRootBranch localPath user repo treeish = do
  liftIO $ do
    wd <- getCurrentDirectory
    exists <- doesDirectoryExist $ localPath
    let uri = githubUri user repo
    when (not exists) $ shallowClone uri localPath
    setCurrentDirectory $ localPath
    shallowPull uri treeish
    setCurrentDirectory wd
  getRootBranch localPath

githubUri :: Text -> Text -> Text
githubUri user repo = "git@github.com:" <> user <> "/" <> repo <> ".git"

shallowClone :: Text -> FilePath -> IO ()
shallowClone uri localPath = "git" ["clone", "--depth=1", uri, Text.pack localPath]

shallowPull :: Text -> Text -> IO ()
shallowPull uri treeish = do
  "git" ["fetch", "--depth=1", uri, treeish]
  "git" ["checkout", treeish]

pushGithubRootBranch
  :: MonadError Err m
  => MonadIO m => CodebasePath -> Text -> Text -> Text -> Branch m -> m ()
pushGithubRootBranch localPath user repo ghbranch b = do
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
