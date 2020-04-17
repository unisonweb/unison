{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Editor.Git
  ( pullGitBranch
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
import           System.FilePath                ( (</>) )
import           Unison.Codebase.GitError
import qualified Unison.Codebase               as Codebase
import           Unison.Codebase                ( Codebase )
import           Unison.Codebase.FileCodebase  as FC
import           Unison.Codebase.Branch         ( Branch
                                                , headHash
                                                )
import qualified Unison.Util.Exception         as Ex
import qualified Unison.Codebase.Branch        as Branch
import qualified Unison.Names3                 as Names
import Unison.Codebase.ShortBranchHash (ShortBranchHash)
import UnliftIO.IO (hFlush, stdout)
import UnliftIO.Directory (getXdgDirectory, XdgDirectory(XdgCache), doesDirectoryExist, findExecutable, removeDirectoryRecursive)
import Unison.Codebase.FileCodebase.Common (encodeFileName)

tempGitDir :: MonadIO m => Text -> m FilePath
tempGitDir url =
  getXdgDirectory XdgCache
    $   "unisonlanguage"
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

-- Given a local path, a remote git repo url, and branch/commit hash,
-- checks for git, the repo path, performs a clone, and verifies resulting repo
pullBranch
  :: (MonadIO m, MonadError GitError m)
  => Text -> Maybe Text -> m CodebasePath
pullBranch _uri (Just t) = error $
  "InputPatterns.parseUri was expected to have prevented you " ++
  "from supplying the git treeish `" ++ Text.unpack t ++ "`!"
pullBranch uri Nothing = do
  checkForGit

  localPath <- tempGitDir uri

  ifM (doesDirectoryExist localPath)
    -- try to update existing directory
    (ifM (isGitRepo localPath)
      (checkoutExisting localPath)
      (throwError (UnrecognizableCacheDir uri localPath)))
    -- directory doesn't exist, so clone anew
    (checkOutNew localPath Nothing)
  pure localPath
  where
  checkoutExisting :: (MonadIO m, MonadError GitError m) => FilePath -> m ()
  checkoutExisting localPath =
    ifM (isEmptyGitRepo localPath)
      -- I don't know how to properly update from an empty repo;
      -- but if this copy is empty, then the remote might be too,
      -- so this impl. just wipes the cache dir and starts over from scratch.
      (do wipeDir localPath; checkOutNew localPath Nothing)
    -- Otherwise proceed!
    (withStatus ("Updating cached copy of " ++ Text.unpack uri ++ " ...") $ do
      gitIn localPath ["reset", "--hard", "--quiet", "HEAD"]
      gitIn localPath ["clean", "-d", "--force", "--quiet"]
      gitIn localPath ["pull", "--force", "--quiet"])

  isEmptyGitRepo :: MonadIO m => FilePath -> m Bool
  isEmptyGitRepo localPath = liftIO $
    -- if rev-parse succeeds, the repo is not empty
    (gitIn localPath ["rev-parse", "--verify", "--quiet", "HEAD"] $> False)
      $? pure True

  checkOutNew localPath branch = do
    withStatus ("Downloading from " ++ Text.unpack uri ++ " ...")
      -- if `treeish` is a branch or tag
      (liftIO $
        "git" $^ (["clone", "--quiet"] ++ ["--depth", "1"]
         ++ maybe [] (\t -> ["--branch", t]) branch
         ++ [uri, Text.pack localPath]))
        `onError` throwError (NoRemoteRepoAt uri)
    isGitDir <- liftIO $ isGitRepo localPath
    unless isGitDir . throwError $ UnrecognizableCheckoutDir uri localPath

  wipeDir localPath = do
    e <- Ex.tryAny . whenM (doesDirectoryExist localPath) $
      removeDirectoryRecursive localPath
    case e of
      Left e -> throwError (SomeOtherError (Text.pack (show e)))
      Right _ -> pure ()

-- pull repo & load arbitrary branch
-- if `sbh` is supplied, we try to load the specified branch hash;
-- otherwise we try to load the root branch
pullGitBranch
  :: forall m v a
   . MonadIO m
  => Codebase m v a
  -> Text
  -> Maybe Text
  -> Maybe ShortBranchHash
  -> ExceptT GitError m (Branch m)
pullGitBranch codebase url treeish sbh = do
  remotePath <- pullBranch url treeish
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

isGitRepo :: MonadIO m => FilePath -> m Bool
isGitRepo dir = liftIO $
  (True <$ gitIn dir ["rev-parse"]) $? pure False

onError :: MonadError e m => MonadIO m => IO () -> m () -> m ()
onError x k = liftIO ((const True <$> x) $? pure False) >>= \case
  True  -> pure ()
  False -> k

setupGitDir :: FilePath -> [Text]
setupGitDir localPath = 
  ["--git-dir", Text.pack $ localPath </> ".git"
  ,"--work-tree", Text.pack localPath]

gitIn :: MonadIO m => FilePath -> [Text] -> m ()
gitIn localPath args = liftIO $ "git" $^ (setupGitDir localPath <> args)

gitTextIn :: MonadIO m => FilePath -> [Text] -> m Text
gitTextIn localPath args = liftIO $ "git" $| setupGitDir localPath <> args

-- Clone the given remote repo and commit to the given local path.
-- Then given a codebase and a branch, write the branch and all its
-- dependencies to the path, then commit and push to the remote repo.
pushGitRootBranch
  :: MonadIO m
  => MonadCatch m
  => Codebase m v a
  -> Branch m
  -> Text
  -> Maybe Text
  -> ExceptT GitError m ()
pushGitRootBranch codebase branch url gitbranch = do
  -- Clone and check out the remote repo
  remotePath <- pullBranch url gitbranch
  -- Stick our changes in the checked-out copy
  merged <-
    withStatus ("Staging files for upload to " ++ Text.unpack url ++ " ...") $
      lift $ Codebase.syncToDirectory codebase remotePath branch
  -- `isBefore` = "is fast-forward merge"
  isBefore <- lift $ Branch.before merged branch
  let mergednames = Branch.toNames0 (Branch.head merged)
      localnames  = Branch.toNames0 (Branch.head branch)
      diff = Names.diff0 localnames mergednames
  when (not isBefore) $
    throwError (PushDestinationHasNewStuff url gitbranch diff)
  let
    -- Commit our changes
    push = do
      -- has anything changed?
      status <- gitTextIn remotePath ["status", "--short"]
      unless (Text.null status) $ do
        gitIn remotePath ["add", "--all", "."]
        gitIn remotePath
          ["commit", "-q", "-m", "Sync branch " <> Text.pack (show $ headHash branch)]
        -- Push our changes to the repo
        case gitbranch of
          Nothing        -> gitIn remotePath ["push", "--quiet", url]
          Just gitbranch -> gitIn remotePath ["push", "--quiet", url, gitbranch]
  withStatus ("Uploading to " ++ Text.unpack url ++ " ...") $
    liftIO push `onException` throwError (NoRemoteRepoAt url)
