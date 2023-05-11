-- | @clone@-related input handlers
module Unison.Codebase.Editor.HandleInput.ProjectClone
  ( handleClone,
  )
where

import Control.Lens (traverseOf, (^.))
import Control.Monad.Reader (ask)
import Data.These (These (..))
import qualified Data.UUID.V4 as UUID
import U.Codebase.Sqlite.DbId (ProjectBranchId (..), ProjectId (..))
import qualified U.Codebase.Sqlite.DbId as Sqlite
import qualified U.Codebase.Sqlite.Project as Sqlite (Project)
import qualified U.Codebase.Sqlite.ProjectBranch as Sqlite
import qualified U.Codebase.Sqlite.Queries as Queries
import Unison.Cli.Monad (Cli)
import qualified Unison.Cli.Monad as Cli
import qualified Unison.Cli.MonadUtils as Cli (stepAt)
import Unison.Cli.ProjectUtils (projectBranchPath)
import qualified Unison.Cli.ProjectUtils as ProjectUtils
import qualified Unison.Cli.Share.Projects as Share
import qualified Unison.Codebase as Codebase
import qualified Unison.Codebase.Branch as Branch
import qualified Unison.Codebase.Editor.HandleInput.Pull as HandleInput.Pull
import qualified Unison.Codebase.Editor.Output as Output
import qualified Unison.Codebase.Path as Path
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectAndBranchNames (..), ProjectBranchName, ProjectName, projectNameUserSlug)
import qualified Unison.Share.API.Hash as Share.API
import qualified Unison.Share.Sync as Share (downloadEntities)
import qualified Unison.Share.Sync.Types as Share
import qualified Unison.Sqlite as Sqlite
import Unison.Sync.Common (hash32ToCausalHash)
import qualified Unison.Sync.Types as Share
import Witch (unsafeFrom)

data LocalProjectKey
  = LocalProjectKey'Name ProjectName
  | LocalProjectKey'Project Sqlite.Project

data RemoteNames
  = RemoteNames'Ambiguous ProjectName (ProjectAndBranch RemoteProjectKey ProjectBranchName)
  | RemoteNames'Unambiguous (ProjectAndBranch RemoteProjectKey ProjectBranchName)

data RemoteProjectKey
  = RemoteProjectKey'Id Sqlite.RemoteProjectId
  | RemoteProjectKey'Name ProjectName

-- | Clone a remote branch.
--
-- There are three ways to call this command unambiguously:
--
--   1. clone /<branch>
--
--       This clones the remote branch called <branch> that belongs the remote project associated with this branch,
--       falling back on this branch's nearest ancestor with an associated remote branch, and if there are none,
--       finally falling back on a name comparison (e.g. trying to clone branch "foo" while in local project named
--       "@bar/baz" will look for branch "@bar/baz/foo" on the server).
--
--       This creates a new branch called <branch> in the current project.
--
--   2. clone <project>/
--
--        This clones the "main" branch of the remote project called <project>, and (creating a local project if
--        necessary) puts it in a local project of the same name. The current project is irrelevant.
--
--   3. clone <project>/<branch>
--
--        This is like (2), but specifying some branch explicitly.
--
-- And a fourth way to call this command ambiguously:
--
--   4. clone <thing> (no leading or trailing slash)
--
--        This will make a best-effort as to what the user meant: if either `clone /<thing>` or `clone <thing>`
--        (treating the argument as a branch or project, respectively) would succeed, but not both, then do that.
--        Otherwise, complain about the ambiguity.
--
--        Note that this means (4) is like (2) when outside of the context of a project branch, because there is no
--        current project, so `clone /<thing>` wouldn't work.
--
--        Note that this also means (4) is like (2) when <thing>, though technically a valid project name, doesn't
--        have a user slug, and so thus can't be cloned from Share (support for other servers that may host project
--        names that don't begin with a user slug is being neglected for convenience here).
--
--        For example, `clone foo` always means `clone /foo`, never `clone foo/`, since "foo" isn't a valid project
--        name on Share (but e.g. "@user/foo" is).

-- The full version of the clone command will take an optional second argument, to all the user to give a different
-- local name to the cloned thing.
--
-- In the meantime, the one argument `clone X` is equivalent to the hypothetical `clone X X`. So, as it stands, it is
-- not possible to (for example) clone one branch into "another" project (using scare quotes because local and remote
-- projects are of course not in a tight one-to-one correspondence).
--
-- For example, I might want to run
--
--   foo/main> clone @unison/base/main /basey
--
-- in order to create a new branch `basey` in the current project, initialized with the contents of the `main` branch
-- of remote project `@unison/base`.
--
-- The clone command can't do this yet; the workaround is to first create an empty branch with `branch.empty`, then
-- `pull`:
--
--   foo/main> branch.empty basey
--   foo/basey> pull @unison/base/main
handleClone :: ProjectAndBranchNames -> Maybe ProjectAndBranchNames -> Cli ()
handleClone remoteNames localNames =
  handleClone2 (fromMaybe remoteNames localNames) remoteNames

handleClone2 :: ProjectAndBranchNames -> ProjectAndBranchNames -> Cli ()
handleClone2 localNames0 remoteNames0 = do
  maybeCurrentProjectBranch <- ProjectUtils.getCurrentProjectBranch

  -- First, do a quick processing of the remote names that doesn't require hitting the network.
  --
  --   <project>/           ==>   abort if <project> doesn't have a user slug
  --
  --   /<branch>            ==>   abort if not in a project
  --
  --   <project>/<branch>   ==>   abort if <project> doesn't have a user slug
  --
  --   <thing>              ==>   if we're not in a project, then treat as if it was <thing>/
  --
  --                              otherwise, if <thing> doesn't have a user slug, treat it as /<thing>
  --
  --                              otherwise, leave it ambiguous for now. this isn't necessarily an error - we will hit
  --                              the server, and if <thing>/ xor /<thing> was valid (e.g. cloning the "@runar/topic"
  --                              project-or-branch, where the "@runar/topic" branch does exist in the project in
  --                              question, and the "@runar/topic" project does not exist), we'll do that
  --
  -- We emerge from this step with either an ambiguous project-or-branch or an unambiguous project-and-branch (the
  -- branch name being locally defaulted to "main" because we don't yet have an API for asking Share for the default
  -- branch of a project).
  remoteNames <- do
    let assertProjectNameHasUserSlug projectName =
          void $
            projectNameUserSlug projectName
              & onNothing (Cli.returnEarly (Output.ProjectNameRequiresUserSlug projectName))
    let -- Return the remote project id associated with the given project branch
        projectBranchRemoteProjectKey :: ProjectAndBranch Sqlite.Project Sqlite.ProjectBranch -> Sqlite.Transaction RemoteProjectKey
        projectBranchRemoteProjectKey (ProjectAndBranch currentProject currentBranch) = do
          Queries.loadRemoteProjectBranch currentProjectId Share.hardCodedUri currentBranchId <&> \case
            Nothing -> RemoteProjectKey'Name (currentProject ^. #name)
            Just (remoteProjectId, _) -> RemoteProjectKey'Id remoteProjectId
          where
            currentProjectId = currentProject ^. #projectId
            currentBranchId = currentBranch ^. #branchId
    let f = \case
          ProjectAndBranchNames'Ambiguous remoteProjectName remoteBranchName ->
            case maybeCurrentProjectBranch of
              Nothing -> f (ProjectAndBranchNames'Unambiguous (This remoteProjectName))
              Just currentProjectBranch ->
                case projectNameUserSlug remoteProjectName of
                  Nothing -> f (ProjectAndBranchNames'Unambiguous (That remoteBranchName))
                  Just _ -> do
                    branchProjectKey <- Cli.runTransaction (projectBranchRemoteProjectKey currentProjectBranch)
                    pure (RemoteNames'Ambiguous remoteProjectName (ProjectAndBranch branchProjectKey remoteBranchName))
          ProjectAndBranchNames'Unambiguous (This remoteProjectName) -> do
            assertProjectNameHasUserSlug remoteProjectName
            let remoteProjectKey = RemoteProjectKey'Name remoteProjectName
            let remoteBranchName = unsafeFrom @Text "main"
            pure (RemoteNames'Unambiguous (ProjectAndBranch remoteProjectKey remoteBranchName))
          ProjectAndBranchNames'Unambiguous (That remoteBranchName) -> do
            currentProjectBranch <- maybeCurrentProjectBranch & onNothing (Cli.returnEarly Output.NotOnProjectBranch)
            remoteProjectKey <- Cli.runTransaction (projectBranchRemoteProjectKey currentProjectBranch)
            pure (RemoteNames'Unambiguous (ProjectAndBranch remoteProjectKey remoteBranchName))
          ProjectAndBranchNames'Unambiguous (These remoteProjectName remoteBranchName) -> do
            assertProjectNameHasUserSlug remoteProjectName
            let remoteProjectKey = RemoteProjectKey'Name remoteProjectName
            pure (RemoteNames'Unambiguous (ProjectAndBranch remoteProjectKey remoteBranchName))
    f remoteNames0

  -- Next, try to resolve the local names to an actual local project (which may not exist yet), aborting on nonsense
  -- inputs:
  --
  --   <project>/           ==>   if we already know the remote branch name is <branch>, abort if <project>/<branch>
  --                              already exists
  --
  --   /<branch>            ==>   abort if not in a project
  --
  --                              abort if <branch> already exists in this project
  --
  --   <project>/<branch>   ==>   abort if <project>/<branch> already exists
  --
  --   <thing>              ==>   if we're not in a project, then treat as if it was <thing>/
  --
  --                              otherwise, <thing> is ambiguous, as we don't know if the user wants to clone into
  --                              <thing>/<branch> (where <branch> is determined by the name of the remote branch we
  --                              are cloning), or /<thing>
  --
  -- Thus, we emerge from this section definitely knowing the local project, and maybe knowing the local branch name
  -- (because it may depend on a network hit to Share that we haven't done yet - trying to get local checks out of the
  -- way first).
  localNames1 <- do
    let knownBranch project branch = do
          void (Cli.runEitherTransaction (assertLocalProjectBranchDoesntExist (ProjectAndBranch project branch)))
          pure (ProjectAndBranch project (Just branch))
    let f names =
          case names of
            ProjectAndBranchNames'Ambiguous localProjectName localBranchName ->
              case maybeCurrentProjectBranch of
                Nothing -> f (ProjectAndBranchNames'Unambiguous (This localProjectName))
                Just (ProjectAndBranch currentProject _) -> do
                  let maybeRemoteBranchName =
                        case remoteNames of
                          RemoteNames'Ambiguous _ _ -> Nothing
                          RemoteNames'Unambiguous (ProjectAndBranch _ remoteBranchName) -> Just remoteBranchName
                  Cli.returnEarly $
                    Output.AmbiguousCloneLocal
                      (ProjectAndBranch localProjectName maybeRemoteBranchName)
                      (ProjectAndBranch (currentProject ^. #name) localBranchName)
            ProjectAndBranchNames'Unambiguous (This localProjectName) ->
              case remoteNames of
                RemoteNames'Ambiguous _ _ -> pure (ProjectAndBranch (LocalProjectKey'Name localProjectName) Nothing)
                RemoteNames'Unambiguous (ProjectAndBranch _ remoteBranchName) ->
                  knownBranch (LocalProjectKey'Name localProjectName) remoteBranchName
            ProjectAndBranchNames'Unambiguous (That localBranchName) -> do
              ProjectAndBranch currentProject _ <-
                maybeCurrentProjectBranch & onNothing (Cli.returnEarly Output.NotOnProjectBranch)
              knownBranch (LocalProjectKey'Project currentProject) localBranchName
            ProjectAndBranchNames'Unambiguous (These localProjectName localBranchName) -> do
              knownBranch (LocalProjectKey'Name localProjectName) localBranchName
    f localNames0

  -- Now we're ready to actually fetch the remote project branch (or, in the case of ambiguous input, two project
  -- branches). We can then (if necessary) resolve the local branch name, in the case that it depends on the remote
  -- branch name.
  (remoteProjectBranch, localNames2) <-
    case remoteNames of
      -- For now: complain about ambiguous remote names
      -- Planned feature: instead of failing right away, actually check whether the project and branch exist, and if
      -- only one does, succeed.
      RemoteNames'Ambiguous remoteProjectName remoteBranchInfo0 -> do
        remoteBranchInfo <-
          remoteBranchInfo0 & traverseOf #project \case
            RemoteProjectKey'Id remoteProjectId ->
              Cli.runTransaction (Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri)
            RemoteProjectKey'Name remoteProjectName -> pure remoteProjectName
        Cli.returnEarly (Output.AmbiguousCloneRemote remoteProjectName remoteBranchInfo)
      RemoteNames'Unambiguous (ProjectAndBranch remoteProjectKey remoteBranchName) -> do
        remoteProjectBranch <-
          case remoteProjectKey of
            RemoteProjectKey'Id remoteProjectId -> do
              remoteProjectName <- Cli.runTransaction (Queries.expectRemoteProjectName remoteProjectId Share.hardCodedUri)
              ProjectUtils.expectRemoteProjectBranchByName (ProjectAndBranch (remoteProjectId, remoteProjectName) remoteBranchName)
            RemoteProjectKey'Name remoteProjectName ->
              ProjectUtils.expectRemoteProjectBranchByNames (ProjectAndBranch remoteProjectName remoteBranchName)
        localNames2 <-
          case localNames1 of
            ProjectAndBranch localProjectKey (Just localBranchName) ->
              pure (ProjectAndBranch localProjectKey localBranchName)
            ProjectAndBranch localProjectKey Nothing -> do
              let localProjectBranch = ProjectAndBranch localProjectKey remoteBranchName
              void (Cli.runEitherTransaction (assertLocalProjectBranchDoesntExist localProjectBranch))
              pure localProjectBranch
        pure (remoteProjectBranch, localNames2)

  cloneInto localNames2 remoteProjectBranch

-- `cloneInto command local remote` clones `remote` into `local`, which is believed to not exist yet, but may (because
-- it takes some time to pull the remote).
cloneInto :: ProjectAndBranch LocalProjectKey ProjectBranchName -> Share.RemoteProjectBranch -> Cli ()
cloneInto localProjectBranch remoteProjectBranch = do
  let remoteProjectName = remoteProjectBranch ^. #projectName
  let remoteBranchName = remoteProjectBranch ^. #branchName

  -- Pull the remote branch's contents
  let remoteBranchHeadJwt = remoteProjectBranch ^. #branchHead
  (result, numDownloaded) <-
    Cli.with HandleInput.Pull.withEntitiesDownloadedProgressCallback \(downloadedCallback, getNumDownloaded) -> do
      result <-
        Share.downloadEntities
          Share.hardCodedBaseUrl
          (Share.RepoInfo (into @Text (ProjectAndBranch remoteProjectName remoteBranchName)))
          remoteBranchHeadJwt
          downloadedCallback
      numDownloaded <- liftIO getNumDownloaded
      pure (result, numDownloaded)
  case result of
    Left err0 ->
      (Cli.returnEarly . Output.ShareError) case err0 of
        Share.SyncError err -> Output.ShareErrorDownloadEntities err
        Share.TransportError err -> Output.ShareErrorTransport err
    Right () -> Cli.respond (Output.DownloadedEntities numDownloaded)

  localProjectAndBranch <-
    Cli.runEitherTransaction do
      -- Repeat the check from before, because (although it's highly unlikely) we could have a name conflict after
      -- downloading the remote branch
      assertLocalProjectBranchDoesntExist localProjectBranch >>= \case
        Left err -> pure (Left err)
        Right maybeLocalProject -> do
          -- Create the local project (if necessary), and create the local branch
          localProjectId <-
            case maybeLocalProject of
              Left localProjectName -> do
                localProjectId <- Sqlite.unsafeIO (ProjectId <$> UUID.nextRandom)
                Queries.insertProject localProjectId localProjectName
                pure localProjectId
              Right localProject -> pure (localProject ^. #projectId)
          localBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
          Queries.insertProjectBranch
            Sqlite.ProjectBranch
              { projectId = localProjectId,
                branchId = localBranchId,
                name = localProjectBranch ^. #branch,
                parentBranchId = Nothing
              }
          Queries.insertBranchRemoteMapping
            localProjectId
            localBranchId
            (remoteProjectBranch ^. #projectId)
            Share.hardCodedUri
            (remoteProjectBranch ^. #branchId)
          pure (Right (ProjectAndBranch localProjectId localBranchId))

  -- Manipulate the root namespace and cd
  Cli.Env {codebase} <- ask
  let branchHead = hash32ToCausalHash (Share.API.hashJWTHash remoteBranchHeadJwt)
  theBranch <- liftIO (Codebase.expectBranchForHash codebase branchHead)
  let path = projectBranchPath localProjectAndBranch
  Cli.stepAt
    ("clone " <> into @Text (ProjectAndBranch remoteProjectName remoteBranchName))
    (Path.unabsolute path, const (Branch.head theBranch))
  Cli.cd path

-- Assert that a local project+branch with this name doesn't already exist. If it does exist, we can't clone over it.
assertLocalProjectBranchDoesntExist ::
  ProjectAndBranch LocalProjectKey ProjectBranchName ->
  Sqlite.Transaction (Either Output.Output (Either ProjectName Sqlite.Project))
assertLocalProjectBranchDoesntExist = \case
  ProjectAndBranch (LocalProjectKey'Name projectName) branchName ->
    Queries.loadProjectByName projectName >>= \case
      Nothing -> pure (Right (Left projectName))
      Just project -> go project branchName
  ProjectAndBranch (LocalProjectKey'Project project) branchName -> go project branchName
  where
    go project branchName =
      Queries.projectBranchExistsByName (project ^. #projectId) branchName <&> \case
        False -> Right (Right project)
        True -> Left (Output.ProjectAndBranchNameAlreadyExists (ProjectAndBranch (project ^. #name) branchName))
