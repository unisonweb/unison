-- | @branch@ input handler
module Unison.Codebase.Editor.HandleInput.Branch
  ( CreateFrom (..),
    CreateFromMergeSource (..),
    handleBranch,
    createBranch,
  )
where

import Control.Monad.Reader
import Data.Map.Strict qualified as Map
import Data.UUID.V4 qualified as UUID
import Network.URI (URI)
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.DbId
import U.Codebase.Sqlite.Project qualified as Sqlite
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite
import U.Codebase.Sqlite.Queries qualified as Q
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.Share.Projects.Types (RemoteProjectBranch (..))
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Input qualified as Input
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Name (Name)
import Unison.Prelude
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectBranchNameKind (..), ProjectName, classifyProjectBranchName)
import Unison.Sqlite qualified as Sqlite

data CreateFrom
  = CreateFrom'NamespaceWithParent Sqlite.ProjectBranch (Branch IO)
  | CreateFrom'ParentBranch Sqlite.ProjectBranch
  | CreateFrom'Namespace (Branch IO)
  | CreateFrom'CausalHash CausalHash
  | -- A merge failed (from local branch, remote branch, or remote loose code), and we're making a branch for the user
    -- to resolve the merge on
    CreateFrom'MergeParents
      (CreateFromMergeSource, CausalHash, Map Name Text {- unique type name to guid -}) -- source
      (Sqlite.ProjectBranch, CausalHash, Map Name Text {- unique type name to guid -}) -- target
      (Branch IO) -- merge branch
  | -- An update failed, and we're making a branch for the user to complete the update on
    CreateFrom'Update Sqlite.ProjectBranch (Branch IO)
  | CreateFrom'Nothingness

data CreateFromMergeSource
  = CreateFromMergeSource'Local Sqlite.ProjectBranch
  | CreateFromMergeSource'Remote RemoteProjectBranch URI
  | CreateFromMergeSource'LooseCode

-- | Create a new project branch from an existing project branch or namespace.
handleBranch :: Input.BranchSourceI -> ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleBranch sourceI projectAndBranchNames@(ProjectAndBranch mayProjectName newBranchName) = do
  -- You can only create release branches with `branch.clone`
  --
  -- We do allow creating draft release branches with `branch`, but you'll get different output if you use
  -- `release.draft`
  case classifyProjectBranchName newBranchName of
    ProjectBranchNameKind'Contributor _user _name -> pure ()
    ProjectBranchNameKind'DraftRelease _ver -> pure ()
    ProjectBranchNameKind'Release ver ->
      Cli.returnEarly (Output.CannotCreateReleaseBranchWithBranchCommand newBranchName ver)
    ProjectBranchNameKind'NothingSpecial -> pure ()

  currentProjectName <- Cli.getCurrentProjectPath <&> view (#project . #name)
  let projectName = (fromMaybe currentProjectName mayProjectName)
  destProject <- do
    Cli.runTransactionWithRollback
      \rollback -> do
        Queries.loadProjectByName projectName & onNothingM do
          -- We can't make the *first* branch of a project with `branch`; the project has to already exist.
          rollback (Output.LocalProjectBranchDoesntExist (ProjectAndBranch projectName newBranchName))

  -- Compute what we should create the branch from.
  maySrcProjectAndBranch <-
    case sourceI of
      Input.BranchSourceI'CurrentContext -> Just . view PP.projectAndBranch_ <$> Cli.getCurrentProjectPath
      Input.BranchSourceI'Empty -> pure Nothing
      Input.BranchSourceI'UnresolvedProjectBranch unresolvedProjectBranch -> do
        pp <- Cli.getCurrentProjectPath
        Just <$> ProjectUtils.resolveProjectBranchInProject (pp ^. #project) (unresolvedProjectBranch & #branch %~ Just)

  case maySrcProjectAndBranch of
    Just srcProjectAndBranch -> do
      let description = "Branch created from " <> into @Text (srcProjectAndBranch & bimap (view #name) (view #name))
      void $ createBranch description (CreateFrom'ParentBranch (view #branch srcProjectAndBranch)) destProject (pure newBranchName)
    Nothing -> do
      let description = "Empty branch created"
      void $ createBranch description CreateFrom'Nothingness destProject (pure newBranchName)

  Cli.respond $
    Output.CreatedProjectBranch
      ( case maySrcProjectAndBranch of
          Just sourceBranch ->
            if sourceBranch ^. #project . #projectId == destProject ^. #projectId
              then Output.CreatedProjectBranchFrom'ParentBranch (sourceBranch ^. #branch . #name)
              else Output.CreatedProjectBranchFrom'OtherBranch sourceBranch
          Nothing -> Output.CreatedProjectBranchFrom'Nothingness
      )
      (projectAndBranchNames & #project .~ projectName)

-- | @createBranch description createFrom project getNewBranchName@:
--
--   1. Creates a new branch row in @project@ at the name from @getNewBranchName@ (failing if branch already exists in @project@).
--   2. Switches to the new branch.
--
-- This bit of functionality is factored out from the main 'handleBranch' handler because it is also called by the
-- @release.draft@ command, which essentially just creates a branch, but with some different output for the user.
--
-- Returns the branch id and name of the newly-created branch.
createBranch ::
  Text ->
  CreateFrom ->
  Sqlite.Project ->
  Sqlite.Transaction ProjectBranchName ->
  Cli (ProjectBranchId, ProjectBranchName)
createBranch description createFrom project getNewBranchName = do
  let projectId = project ^. #projectId
  Cli.Env {codebase} <- ask
  (mayParentBranchId, newBranchCausalHashId) <- case createFrom of
    CreateFrom'ParentBranch parentBranch -> Cli.runTransaction do
      newBranchCausalHashId <- Q.expectProjectBranchHead parentBranch.projectId parentBranch.branchId
      let parentBranchId = if parentBranch.projectId == projectId then Just parentBranch.branchId else Nothing
      pure (parentBranchId, newBranchCausalHashId)
    CreateFrom'Nothingness -> Cli.runTransaction do
      (_, causalHashId) <- Codebase.emptyCausalHash
      pure (Nothing, causalHashId)
    CreateFrom'NamespaceWithParent parentBranch namespace -> do
      liftIO $ Codebase.putBranch codebase namespace
      Cli.runTransaction do
        newBranchCausalHashId <- Q.expectCausalHashIdByCausalHash (Branch.headHash namespace)
        let parentBranchId = if parentBranch.projectId == projectId then Just parentBranch.branchId else Nothing
        pure (parentBranchId, newBranchCausalHashId)
    CreateFrom'MergeParents _ (targetBranch, _, _) namespace -> do
      liftIO $ Codebase.putBranch codebase namespace
      Cli.runTransaction do
        newBranchCausalHashId <- Q.expectCausalHashIdByCausalHash (Branch.headHash namespace)
        pure (Just targetBranch.branchId, newBranchCausalHashId)
    CreateFrom'Update parentBranch namespace -> do
      liftIO $ Codebase.putBranch codebase namespace
      Cli.runTransaction do
        newBranchCausalHashId <- Q.expectCausalHashIdByCausalHash (Branch.headHash namespace)
        pure (Just parentBranch.branchId, newBranchCausalHashId)
    CreateFrom'CausalHash causalHash -> do
      Cli.runTransaction do
        causalHashId <- Q.expectCausalHashIdByCausalHash causalHash
        pure (Nothing, causalHashId)
    CreateFrom'Namespace branch -> do
      liftIO $ Codebase.putBranch codebase branch
      Cli.runTransaction do
        newBranchCausalHashId <- Q.expectCausalHashIdByCausalHash (Branch.headHash branch)
        pure (Nothing, newBranchCausalHashId)
  (newBranchName, newBranchId) <-
    Cli.runTransactionWithRollback \rollback -> do
      newBranchName <- getNewBranchName
      Queries.projectBranchExistsByName projectId newBranchName >>= \case
        True -> rollback (Output.ProjectAndBranchNameAlreadyExists (ProjectAndBranch (project ^. #name) newBranchName))
        False -> do
          -- Here, we are forking to `foo/bar`, where project `foo` does exist, and it does not have a branch named
          -- `bar`, so the fork will succeed.
          newBranchId <- Sqlite.unsafeIO (ProjectBranchId <$> UUID.nextRandom)
          Queries.insertProjectBranch
            description
            newBranchCausalHashId
            Sqlite.ProjectBranch
              { projectId,
                branchId = newBranchId,
                name = newBranchName,
                parentBranchId = mayParentBranchId
              }
          case createFrom of
            CreateFrom'MergeParents
              (source, sourceCausalHash, sourceUniqueTypeGuids)
              (targetBranch, targetCausalHash, targetUniqueTypeGuids)
              _ -> do
                sourceCausalHashId <- Queries.expectCausalHashIdByCausalHash sourceCausalHash
                targetCausalHashId <- Queries.expectCausalHashIdByCausalHash targetCausalHash
                case source of
                  CreateFromMergeSource'Local sourceBranch -> do
                    Queries.insertMergeBranchLocal
                      targetBranch.projectId
                      newBranchId
                      (sourceBranch.branchId, sourceCausalHashId)
                      (targetBranch.branchId, targetCausalHashId)
                  CreateFromMergeSource'Remote sourceBranch sourceHost -> do
                    Queries.insertMergeBranchRemote
                      targetBranch.projectId
                      newBranchId
                      (sourceBranch.projectId, sourceBranch.branchId, sourceHost, sourceCausalHashId)
                      (targetBranch.branchId, targetCausalHashId)
                  CreateFromMergeSource'LooseCode -> do
                    Queries.insertMergeBranchLooseCode
                      targetBranch.projectId
                      newBranchId
                      sourceCausalHashId
                      (targetBranch.branchId, targetCausalHashId)
                -- Create unique type to GUID mapping for source and target namespaces
                let ensureUniqueTypeToGuidMapping uniqueTypeGuids causalHashId =
                      when (not (Map.null uniqueTypeGuids)) do
                        namespaceHashId <- Queries.expectCausalValueHashId causalHashId
                        Queries.existsAnyNamespaceUniqueTypeGuidForNamespace namespaceHashId >>= \case
                          True -> pure ()
                          False ->
                            for_ (Map.toList uniqueTypeGuids) \(name, guid) ->
                              Queries.insertNamespaceUniqueTypeGuid namespaceHashId name guid
                ensureUniqueTypeToGuidMapping sourceUniqueTypeGuids sourceCausalHashId
                ensureUniqueTypeToGuidMapping targetUniqueTypeGuids targetCausalHashId
            CreateFrom'Update _parentBranch _namespace -> do
              pure ()
            _ -> pure ()
          pure (newBranchName, newBranchId)

  Cli.switchProject (ProjectAndBranch projectId newBranchId)
  pure (newBranchId, newBranchName)
