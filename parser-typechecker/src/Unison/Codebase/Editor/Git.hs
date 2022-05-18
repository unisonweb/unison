{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Unison.Codebase.Editor.Git
  ( gitIn,
    gitTextIn,
    gitInCaptured,
    withRepo,
    withIOError,
    withStatus,
    withIsolatedRepo,
    debugGit,
    gitDirToPath,
    GitBranchBehavior (..),
    GitRepo (..),

    -- * Exported for testing
    gitCacheDir,
  )
where

import qualified Control.Exception
import Control.Monad.Except (MonadError, throwError)
import qualified Data.ByteString.Base16 as ByteString
import qualified Data.Char as Char
import qualified Data.Text as Text
import Shellmet (($?), ($^), ($|))
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO.Unsafe (unsafePerformIO)
import Unison.Codebase.Editor.RemoteRepo (ReadGitRepo(..))
import Unison.Codebase.GitError (GitProtocolError)
import qualified Unison.Codebase.GitError as GitError
import Unison.Prelude
import qualified UnliftIO
import UnliftIO.Directory (XdgDirectory (XdgCache), doesDirectoryExist, findExecutable, getXdgDirectory)
import UnliftIO.Environment (lookupEnv)
import UnliftIO.IO (hFlush, stdout)
import qualified UnliftIO.Process as UnliftIO

debugGit :: Bool
debugGit =
  isJust (unsafePerformIO (lookupEnv "UNISON_DEBUG_GIT"))
{-# NOINLINE debugGit #-}

gitVerbosity :: [Text]
gitVerbosity =
  if debugGit
    then []
    else ["--quiet"]

-- https://superuser.com/questions/358855/what-characters-are-safe-in-cross-platform-file-names-for-linux-windows-and-os
encodeFileName :: String -> FilePath
encodeFileName s =
  let go ('.' : rem) = "$dot$" <> go rem
      go ('$' : rem) = "$$" <> go rem
      go (c : rem)
        | elem @[] c "/\\:*?\"<>|" || not (Char.isPrint c && Char.isAscii c) =
          "$x" <> encodeHex [c] <> "$" <> go rem
        | otherwise = c : go rem
      go [] = []
      encodeHex :: String -> String
      encodeHex =
        Text.unpack . Text.toUpper . ByteString.encodeBase16
          . encodeUtf8
          . Text.pack
   in -- 'bare' suffix is to avoid clashes with non-bare repos initialized by earlier versions
      -- of ucm.
      go s <> "-bare"

gitCacheDir :: MonadIO m => Text -> m FilePath
gitCacheDir url =
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

-- | Run an action on an isolated copy of the provided repo.
-- The repo is deleted when the action exits or fails.
-- A branch or tag to check out from the source repo may be specified.
withIsolatedRepo ::
  forall m r.
  (MonadUnliftIO m) =>
  GitRepo ->
  Text ->
  Maybe Text ->
  (GitRepo -> m r) ->
  m (Either GitProtocolError r)
withIsolatedRepo srcPath origin mayGitRef action = do
  UnliftIO.withSystemTempDirectory "ucm-isolated-repo" $ \tempDir -> do
    let tempRepo = Worktree tempDir
    copyCommand tempRepo >>= \case
      Left gitErr -> pure $ Left (GitError.CopyException (gitDirToPath srcPath) tempDir (show gitErr))
      Right () -> Right <$> action tempRepo
  where
    copyCommand :: GitRepo -> m (Either IOException ())
    copyCommand dest = UnliftIO.tryIO . liftIO $ do
      gitGlobal
        ( ["clone", "--origin", "git-cache"]
            -- tags work okay here too.
            ++ maybe [] (\t -> ["--branch", t]) mayGitRef
            ++ [Text.pack . gitDirToPath $ srcPath, Text.pack . gitDirToPath $ dest]
        )
      -- If a specific ref wasn't requested, ensure we have all branches and tags from the source.
      -- This is fast since it's a local fetch.
      when (isNothing mayGitRef) $ do
        -- If the source repo is empty, we can't fetch, but there won't be anything to
        -- fetch anyways.
        unlessM (isEmptyGitRepo srcPath) $ do
          gitIn dest $ ["fetch", "--tags", Text.pack . gitDirToPath $ srcPath] ++ gitVerbosity
      gitIn dest $ ["remote", "add", "origin", origin]

-- | Define what to do if the repo we're pulling/pushing doesn't have the specified branch.
data GitBranchBehavior
  = -- If the desired branch doesn't exist in the repo,
    -- create a new branch by the provided name with a fresh codebase
    CreateBranchIfMissing
  | -- Fail with an error if the branch doesn't exist.
    RequireExistingBranch

-- | Clone or fetch an updated copy of the provided repository and check out the expected ref,
-- then provide the action with a path to the codebase in that repository.
-- Note that the repository provided to the action is temporary, it will be removed when the
-- action completes or fails.
withRepo ::
  forall m a.
  (MonadUnliftIO m) =>
  ReadGitRepo ->
  GitBranchBehavior ->
  (GitRepo -> m a) ->
  m (Either GitProtocolError a)
withRepo repo@(ReadGitRepo {url = uri, ref = mayGitRef}) branchBehavior action = UnliftIO.try $ do
  throwExceptT $ checkForGit
  gitCachePath <- gitCacheDir uri
  -- Ensure we have the main branch in the cache dir no matter what
  throwExceptT $ cloneIfMissing repo {ref = Nothing} gitCachePath
  let gitCacheRepo = Bare gitCachePath
  gitRef <- case mayGitRef of
    Nothing -> fromMaybe "main" <$> getDefaultBranch gitCacheRepo
    Just gitRef -> pure gitRef
  doesRemoteRefExist <- fetchAndUpdateRef gitCacheRepo gitRef
  if doesRemoteRefExist
    then do
      -- A ref by the requested name exists on the remote.
      withStatus ("Checking out " ++ Text.unpack gitRef ++ " ...") $ do
        -- Check out the ref in a new isolated repo
        throwEitherM . withIsolatedRepo gitCacheRepo uri (Just gitRef) $ action
    else do
      -- No ref by the given name exists on the remote
      case branchBehavior of
        RequireExistingBranch -> UnliftIO.throwIO (GitError.RemoteRefNotFound uri gitRef)
        CreateBranchIfMissing ->
          withStatus ("Creating new branch " ++ Text.unpack gitRef ++ " ...")
            . throwEitherM
            . withIsolatedRepo gitCacheRepo uri Nothing
            $ \(workTree) -> do
              -- It's possible for the branch to exist in the cache even if it's not in the
              -- remote, if for instance the branch was deleted from the remote.
              -- In that case we delete the branch from the cache and create a new one.
              localRefExists <- doesLocalRefExist gitCacheRepo gitRef
              when localRefExists $ do
                currentBranch <- gitTextIn workTree ["branch", "--show-current"]
                -- In the rare case where we've got the branch already checked out,
                -- we need to temporarily switch to a different branch so we can delete and
                -- reset the branch to an orphan.
                when (currentBranch == gitRef) $ gitIn workTree $ ["branch", "-B", "_unison_temp_branch"] ++ gitVerbosity
                gitIn workTree $ ["branch", "-D", gitRef] ++ gitVerbosity
              gitIn workTree $ ["checkout", "--orphan", gitRef] ++ gitVerbosity
              -- Checking out an orphan branch doesn't actually clear the worktree, do that manually.
              _ <- gitInCaptured workTree $ ["rm", "--ignore-unmatch", "-rf", "."] ++ gitVerbosity
              action workTree
  where
    -- Check if a ref exists in the repository at workDir.
    doesLocalRefExist :: GitRepo -> Text -> m Bool
    doesLocalRefExist workDir ref = liftIO $ do
      (gitIn workDir (["show-ref", "--verify", ref] ++ gitVerbosity) $> True)
        $? pure False
    -- fetch the given ref and update the local repositories ref to match the remote.
    -- returns whether or not the ref existed on the remote.
    fetchAndUpdateRef :: GitRepo -> Text -> m Bool
    fetchAndUpdateRef workDir gitRef = do
      (succeeded, _, _) <-
        gitInCaptured
          workDir
          ( [ "fetch",
              "--tags", -- if the gitref is a tag, fetch and update that too.
              "--force", -- force updating local refs even if not fast-forward
              -- update local refs with the same name they have on the remote.
              "--refmap",
              "*:*",
              "--depth",
              "1",
              uri, -- The repo to fetch from
              gitRef -- The specific reference to fetch
            ]
              ++ gitVerbosity
          )
      pure succeeded

-- | Do a `git clone` (for a not-previously-cached repo).
cloneIfMissing :: (MonadIO m, MonadError GitProtocolError m) => ReadGitRepo -> FilePath -> m GitRepo
cloneIfMissing repo@(ReadGitRepo {url = uri}) localPath = do
  doesDirectoryExist localPath >>= \case
    True ->
      whenM (not <$> isGitRepo (Bare localPath)) $ do
        throwError (GitError.UnrecognizableCacheDir repo localPath)
    False -> do
      -- directory doesn't exist, so clone anew
      cloneRepo
  pure $ Bare localPath
  where
    cloneRepo = do
      withStatus ("Downloading from " ++ Text.unpack uri ++ " ...") $
        ( liftIO $
            gitGlobal
              ( ["clone"]
                  ++ ["--bare"]
                  ++ ["--depth", "1"]
                  ++ [uri, Text.pack localPath]
              )
        )
          `withIOError` (throwError . GitError.CloneException repo . show)
      isGitDir <- liftIO $ isGitRepo (Bare localPath)
      unless isGitDir . throwError $ GitError.UnrecognizableCheckoutDir repo localPath

-- | See if `git` is on the system path.
checkForGit :: MonadIO m => MonadError GitProtocolError m => m ()
checkForGit = do
  gitPath <- liftIO $ findExecutable "git"
  when (isNothing gitPath) $ throwError GitError.NoGit

-- | Returns the name of the default branch of a repository, if one exists.
getDefaultBranch :: MonadIO m => GitRepo -> m (Maybe Text)
getDefaultBranch dir = liftIO $ do
  (Text.stripPrefix "refs/heads/" <$> gitTextIn dir ["symbolic-ref", "HEAD"])
    $? pure Nothing

-- | Does `git` recognize this directory as being managed by git?
isGitRepo :: MonadIO m => GitRepo -> m Bool
isGitRepo dir =
  liftIO $
    (True <$ gitIn dir (["rev-parse"] ++ gitVerbosity)) $? pure False

-- | Returns True if the repo is empty, i.e. has no commits at the current branch,
-- or if the dir isn't a git repo at all.
isEmptyGitRepo :: (MonadIO m) => GitRepo -> m Bool
isEmptyGitRepo dir = liftIO do
  (gitTextIn dir (["rev-parse", "HEAD"] ++ gitVerbosity) $> False) $? pure True

-- | Perform an IO action, passing any IO exception to `handler`
withIOError :: MonadIO m => IO a -> (IOException -> m a) -> m a
withIOError action handler =
  liftIO (fmap Right action `Control.Exception.catch` (pure . Left))
    >>= either handler pure

-- | A path to a git repository.
data GitRepo
  = Bare FilePath
  | Worktree FilePath
  deriving (Show)

gitDirToPath :: GitRepo -> FilePath
gitDirToPath = \case
  Bare fp -> fp
  Worktree fp -> fp

-- | Generate some `git` flags for operating on some arbitary checked out copy
setupGitDir :: GitRepo -> [Text]
setupGitDir dir =
  case dir of
    Bare localPath ->
      ["--git-dir", Text.pack localPath]
    Worktree localPath ->
      [ "--git-dir",
        Text.pack (localPath </> ".git"),
        "--work-tree",
        Text.pack localPath
      ]

-- | Run a git command in the current work directory.
-- Note: this should only be used for commands like 'clone' which don't interact with an
-- existing repository.
gitGlobal :: MonadIO m => [Text] -> m ()
gitGlobal args = do
  when debugGit $ traceM (Text.unpack . Text.unwords $ ["$ git"] <> args)
  liftIO $ "git" $^ (args ++ gitVerbosity)

-- | Run a git command in the repository at localPath
gitIn :: MonadIO m => GitRepo -> [Text] -> m ()
gitIn localPath args = do
  when debugGit $ traceM (Text.unpack . Text.unwords $ ["$ git"] <> setupGitDir localPath <> args)
  liftIO $ "git" $^ (setupGitDir localPath <> args)

-- | like 'gitIn', but silences all output from the command and returns whether the command
-- succeeded.
gitInCaptured :: MonadIO m => GitRepo -> [Text] -> m (Bool, Text, Text)
gitInCaptured localPath args = do
  when debugGit $ traceM (Text.unpack . Text.unwords $ ["$ git"] <> setupGitDir localPath <> args)
  (exitCode, stdout, stderr) <- UnliftIO.readProcessWithExitCode "git" (Text.unpack <$> setupGitDir localPath <> args) ""
  pure (exitCode == ExitSuccess, Text.pack stdout, Text.pack stderr)

-- | Run a git command in the repository at localPath and capture stdout
gitTextIn :: MonadIO m => GitRepo -> [Text] -> m Text
gitTextIn localPath args = do
  when debugGit $ traceM (Text.unpack . Text.unwords $ ["$ git"] <> setupGitDir localPath <> args)
  liftIO $ "git" $| setupGitDir localPath <> args
