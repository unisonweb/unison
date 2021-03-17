{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.SqliteCodebase.Git
  ( importRemoteBranch
  , pushGitRootBranch
  , viewRemoteBranch
  ) where

import Unison.Prelude
import Unison.Codebase.Editor.Git

import           Control.Monad.Except           ( throwError
                                                , ExceptT
                                                )
import           Control.Monad.Extra            ((||^))
import qualified Data.Text                     as Text
import qualified Data.Validation        as Validation
import qualified Unison.Codebase.GitError      as GitError
import           Unison.Codebase.GitError       (GitError)
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase                (Codebase, CodebasePath)
import           Unison.Codebase.Editor.RemoteRepo ( RemoteRepo(GitRepo)
                                                   , RemoteNamespace
                                                   , printRepo
                                                   )
import qualified Unison.Codebase.SqliteCodebase  as FC
import           Unison.Codebase.Branch         ( Branch
                                                , headHash
                                                )
import qualified Unison.Codebase.Path          as Path
import           Unison.Codebase.SyncMode       ( SyncMode )
import           Unison.Util.Timing             (time)
import qualified Unison.Codebase.Branch        as Branch

-- | Sync elements as needed from a remote codebase into the local one.
-- If `sbh` is supplied, we try to load the specified branch hash;
-- otherwise we try to load the root branch.
importRemoteBranch
  :: forall m v a
   . MonadIO m
  => Codebase m v a
  -> RemoteNamespace
  -> SyncMode
  -> ExceptT GitError m (Branch m)
importRemoteBranch codebase ns mode = do
  (branch, cacheDir) <- viewRemoteBranch' ns
  withStatus "Importing downloaded files into local codebase..." $
    time "SyncFromDirectory" $
      lift $ Codebase.syncFromDirectory codebase cacheDir mode branch
  pure branch

-- | Pull a git branch and view it from the cache, without syncing into the
-- local codebase.
viewRemoteBranch :: forall m. MonadIO m
  => RemoteNamespace -> ExceptT GitError m (Branch m)
viewRemoteBranch = fmap fst . viewRemoteBranch'

viewRemoteBranch' :: forall m. MonadIO m
  => RemoteNamespace -> ExceptT GitError m (Branch m, CodebasePath)
viewRemoteBranch' (repo, sbh, path) = do
  -- set up the cache dir
  remotePath <- time "Git fetch" $ pullBranch repo
  ifM (FC.codebaseExists remotePath)
    (do
      (closeCodebase, codebase) <- lift (FC.sqliteCodebase remotePath) >>=
        Validation.valueOr (\_missingSchema -> throwError $ GitError.CouldntOpenCodebase repo remotePath) . fmap pure
      -- try to load the requested branch from it
      branch <- time "Git fetch (sbh)" $ case sbh of
        -- load the root branch
        Nothing -> lift (Codebase.getRootBranch codebase) >>= \case
          Left Codebase.NoRootBranch -> pure Branch.empty
          Left (Codebase.CouldntLoadRootBranch h) ->
            throwError $ GitError.CouldntLoadRootBranch repo h
          Left (Codebase.CouldntParseRootBranch s) ->
            throwError $ GitError.CouldntParseRootBranch repo s
          Right b -> pure b
        -- load from a specific `ShortBranchHash`
        Just sbh -> do
          branchCompletions <- lift $ Codebase.branchHashesByPrefix codebase sbh
          case toList branchCompletions of
            [] -> throwError $ GitError.NoRemoteNamespaceWithHash repo sbh
            [h] -> (lift $ Codebase.getBranchForHash codebase h) >>= \case
              Just b -> pure b
              Nothing -> throwError $ GitError.NoRemoteNamespaceWithHash repo sbh
            _ -> throwError $ GitError.RemoteNamespaceHashAmbiguous repo sbh branchCompletions
      lift closeCodebase
      pure (Branch.getAt' path branch, remotePath))
    -- else there's no initialized codebase at this repo; we pretend there's an empty one.
    (pure (Branch.empty, remotePath))

-- Given a branch that is "after" the existing root of a given git repo,
-- stage and push the branch (as the new root) + dependencies to the repo.
pushGitRootBranch
  :: MonadIO m
  => Codebase m v a
  -> Branch m
  -> RemoteRepo
  -> SyncMode
  -> ExceptT GitError m ()
pushGitRootBranch codebase branch repo syncMode = do
  -- Pull the remote repo into a staging directory
  (remoteRoot, remotePath) <- viewRemoteBranch' (repo, Nothing, Path.empty)
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
      lift (Codebase.syncToDirectory codebase remotePath syncMode branch)
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
