{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Editor.Git where

import Unison.Prelude

import qualified Control.Exception
import Control.Monad.Except (MonadError, throwError)
import qualified Data.Text as Text
import Shellmet (($?), ($^), ($|))
import System.FilePath ((</>))
import Unison.Codebase (CodebasePath)
import Unison.Codebase.Editor.RemoteRepo (RemoteRepo (GitRepo))
import Unison.Codebase.FileCodebase.Common (encodeFileName)
import Unison.Codebase.GitError (GitError)
import qualified Unison.Codebase.GitError as GitError
import qualified Unison.Util.Exception as Ex
import UnliftIO.Directory (XdgDirectory (XdgCache), doesDirectoryExist, findExecutable, getXdgDirectory, removeDirectoryRecursive)
import UnliftIO.IO (hFlush, stdout)

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

-- | Given a remote git repo url, and branch/commit hash (currently
-- not allowed): checks for git, clones or updates a cached copy of the repo
pullBranch :: (MonadIO m, MonadError GitError m) => RemoteRepo -> m CodebasePath
pullBranch (GitRepo _uri (Just t)) = error $
  "Pulling a specific commit isn't fully implemented or tested yet.\n" ++
  "InputPatterns.parseUri was expected to have prevented you " ++
  "from supplying the git treeish `" ++ Text.unpack t ++ "`!"
pullBranch repo@(GitRepo uri Nothing) = do
  checkForGit
  localPath <- tempGitDir uri
  ifM (doesDirectoryExist localPath)
    -- try to update existing directory
    (ifM (isGitRepo localPath)
      (checkoutExisting localPath)
      (throwError (GitError.UnrecognizableCacheDir uri localPath)))
    -- directory doesn't exist, so clone anew
    (checkOutNew localPath Nothing)
  pure localPath

  where
  -- | Do a `git clone` (for a not-previously-cached repo).
  checkOutNew :: (MonadIO m, MonadError GitError m) => CodebasePath -> Maybe Text -> m ()
  checkOutNew localPath branch = do
    withStatus ("Downloading from " ++ Text.unpack uri ++ " ...") $
      (liftIO $
        "git" $^ (["clone", "--quiet"] ++ ["--depth", "1"]
         ++ maybe [] (\t -> ["--branch", t]) branch
         ++ [uri, Text.pack localPath]))
        `withIOError` (throwError . GitError.CloneException repo . show)
    isGitDir <- liftIO $ isGitRepo localPath
    unless isGitDir . throwError $ GitError.UnrecognizableCheckoutDir uri localPath

  -- | Do a `git pull` on a cached repo.
  checkoutExisting :: (MonadIO m, MonadError GitError m) => FilePath -> m ()
  checkoutExisting localPath =
    ifM (isEmptyGitRepo localPath)
      -- I don't know how to properly update from an empty remote repo.
      -- As a heuristic, if this cached copy is empty, then the remote might
      -- be too, so this impl. just wipes the cached copy and starts from scratch.
      (do wipeDir localPath; checkOutNew localPath Nothing)
    -- Otherwise proceed!
    (withStatus ("Updating cached copy of " ++ Text.unpack uri ++ " ...") $ do
      gitIn localPath ["reset", "--hard", "--quiet", "HEAD"]
      gitIn localPath ["clean", "-d", "--force", "--quiet"]
      gitIn localPath ["pull", "--force", "--quiet"])

  isEmptyGitRepo :: MonadIO m => FilePath -> m Bool
  isEmptyGitRepo localPath = liftIO $
    -- if rev-parse succeeds, the repo is _not_ empty, so return False; else True
    (gitTextIn localPath ["rev-parse", "--verify", "--quiet", "HEAD"] $> False)
      $? pure True

  -- | try removing a cached copy
  wipeDir localPath = do
    e <- Ex.tryAny . whenM (doesDirectoryExist localPath) $
      removeDirectoryRecursive localPath
    case e of
      Left e -> throwError (GitError.SomeOtherError (show e))
      Right _ -> pure ()

-- | See if `git` is on the system path.
checkForGit :: MonadIO m => MonadError GitError m => m ()
checkForGit = do
  gitPath <- liftIO $ findExecutable "git"
  when (isNothing gitPath) $ throwError GitError.NoGit

-- | Does `git` recognize this directory as being managed by git?
isGitRepo :: MonadIO m => FilePath -> m Bool
isGitRepo dir = liftIO $
  (True <$ gitIn dir ["rev-parse"]) $? pure False

-- | Perform an IO action, passing any IO exception to `handler`
withIOError :: MonadIO m => IO a -> (IOException -> m a) -> m a
withIOError action handler =
  liftIO (fmap Right action `Control.Exception.catch` (pure . Left)) >>=
    either handler pure

-- | Generate some `git` flags for operating on some arbitary checked out copy
setupGitDir :: FilePath -> [Text]
setupGitDir localPath =
  ["--git-dir", Text.pack $ localPath </> ".git"
  ,"--work-tree", Text.pack localPath]

gitIn :: MonadIO m => FilePath -> [Text] -> m ()
gitIn localPath args = liftIO $ "git" $^ (setupGitDir localPath <> args)

gitTextIn :: MonadIO m => FilePath -> [Text] -> m Text
gitTextIn localPath args = liftIO $ "git" $| setupGitDir localPath <> args
