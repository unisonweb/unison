{-# LANGUAGE OverloadedStrings #-}

module Unison.Codebase.Editor.Git
  ( pullGitRootBranch
  , pullGitBranch
  , pushGitRootBranch
  ) where

import Unison.Prelude

import           Control.Monad.Catch            ( MonadCatch
                                                , onException
                                                )
import           Control.Monad.Except           ( MonadError
                                                , throwError
                                                , ExceptT
                                                )
import qualified Data.Text                     as Text
import           Shellmet                       ( ($?), ($|), ($^))
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
import UnliftIO.IO (hFlush, stdout)

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

-- Given a local path, a remote git repo url, and branch/commit hash,
-- checks for git, the repo path, performs a clone, and verifies resulting repo
pullBranch
  :: MonadIO m
  => MonadError GitError m => FilePath -> Text -> Maybe Text -> m ()
pullBranch localPath uri treeish = do
  checkForGit
  e <- liftIO . Ex.tryAny . whenM (doesDirectoryExist localPath) $
    removeDirectoryRecursive localPath
  case e of
    Left e -> throwError (SomeOtherError (Text.pack (show e)))
    Right _ -> pure ()
  withStatus ("Downloading from " ++ Text.unpack uri ++ " ...")
    ("git" $^ (["clone", "--quiet"] ++ ["--depth", "1"]
       ++ maybe [] (\t -> ["--branch", t]) treeish
       ++ [uri, Text.pack localPath]))
      `onError` throwError (NoRemoteRepoAt uri)
  isGitDir <- liftIO $ checkGitDir localPath
  unless isGitDir . throwError $ NoLocalRepoAt localPath

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
-- if `loadInfo` is Left, we try to load the root branch;
-- if Right, we try to load the specified hash
pullGitBranch
  :: forall m v a
   . MonadIO m
  => CodebasePath
  -> Codebase m v a
  -> Text
  -> Maybe Text
  -> Maybe ShortBranchHash
  -> ExceptT GitError m (Branch m)
pullGitBranch remotePath codebase url treeish sbh = do
  pullBranch remotePath url treeish
  branch :: (Branch m) <- case sbh of
    Nothing -> lift (FC.getRootBranch remotePath) >>= \case
      Left Codebase.NoRootBranch -> pure Branch.empty
      Left (Codebase.CouldntLoadRootBranch h) ->
        throwError $ Couldn'tLoadRootBranch url treeish sbh h
      Right b -> pure b
    Just sbh -> do
      branchCompletions <- lift $ FC.branchHashesByPrefix remotePath sbh
      case toList branchCompletions of
        [] -> throwError $ NoRemoteNamespaceWithHash url treeish sbh
        [h] -> (lift $ FC.branchFromFiles remotePath h) >>= \case
          Just b -> pure b
          Nothing -> throwError $ NoRemoteNamespaceWithHash url treeish sbh
        _ -> throwError $ RemoteNamespaceHashAmbiguous url treeish sbh branchCompletions
  withStatus "Importing downloaded files into local codebase..." $
    lift $ Codebase.syncFromDirectory codebase remotePath
  pure branch

checkForGit :: MonadIO m => MonadError GitError m => m ()
checkForGit = do
  gitPath <- liftIO $ findExecutable "git"
  when (isNothing gitPath) $ throwError NoGit

checkGitDir :: FilePath -> IO Bool
checkGitDir dir =
  (const True <$> gitIn dir ["rev-parse"]) $? pure False

onError :: MonadError e m => MonadIO m => IO () -> m () -> m ()
onError x k = liftIO ((const True <$> x) $? pure False) >>= \case
  True  -> pure ()
  False -> k

setupGitDir :: FilePath -> [Text]
setupGitDir localPath = 
  ["--git-dir", Text.pack $ localPath </> ".git"
  ,"--work-tree", Text.pack localPath]

gitIn :: FilePath -> [Text] -> IO ()
gitIn localPath args = "git" $^ (setupGitDir localPath <> args)

gitTextIn :: FilePath -> [Text] -> IO Text
gitTextIn localPath args = "git" $| setupGitDir localPath <> args

-- Clone the given remote repo and commit to the given local path.
-- Then given a codebase and a branch, write the branch and all its
-- dependencies to the path, then commit and push to the remote repo.
pushGitRootBranch
  :: MonadIO m
  => MonadCatch m
  => CodebasePath
  -> Codebase m v a
  -> Branch m
  -> Text
  -> Maybe Text
  -> ExceptT GitError m ()
pushGitRootBranch remotePath codebase branch url gitbranch = do
  -- Clone and pull the remote repo
  pullBranch remotePath url gitbranch
  -- Stick our changes in the checked-out copy
  merged <-
    withStatus ("Staging files for upload to " ++ Text.unpack url ++ " ...") $
      lift $ syncToDirectory codebase remotePath branch
  isBefore <- lift $ Branch.before merged branch
  let mergednames = Branch.toNames0 (Branch.head merged)
      localnames  = Branch.toNames0 (Branch.head branch)
      diff = Names.diff0 localnames mergednames
  when (not isBefore) $
    throwError (PushDestinationHasNewStuff url gitbranch diff)
  let
    push = do
      -- Commit our changes
      status <- gitTextIn remotePath ["status", "--short"]
      unless (Text.null status) $ do
        gitIn remotePath ["add", "--all", "."]
        gitIn remotePath
          ["commit", "-q", "-m", "Sync branch " <> Text.pack (show $ headHash branch)]
        -- Push our changes to the repo
        case gitbranch of
          Nothing        -> gitIn remotePath ["push", "--quiet", "--all", url]
          Just gitbranch -> gitIn remotePath ["push", "--quiet", url, gitbranch]
  withStatus ("Uploading to " ++ Text.unpack url ++ " ...") $
    liftIO push `onException` throwError (NoRemoteRepoAt url)
