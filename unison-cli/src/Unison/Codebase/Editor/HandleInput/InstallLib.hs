-- | @lib.install@ input handler
module Unison.Codebase.Editor.HandleInput.InstallLib
  ( handleInstallLib,
  )
where

import Control.Monad.Reader (ask)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Sqlite.Project qualified as Sqlite (Project (..))
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite (ProjectBranch (..))
import Unison.Cli.DownloadUtils
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.Share.Projects qualified as Share
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Core.Project (ProjectBranchName)
import Unison.NameSegment (NameSegment)
import Unison.NameSegment.Internal qualified as NameSegment
import Unison.Prelude
import Unison.Project
  ( ProjectAndBranch (..),
    ProjectBranchNameKind (..),
    ProjectBranchNameOrLatestRelease (..),
    ProjectName,
    Semver (..),
    classifyProjectBranchName,
    projectNameToUserProjectSlugs,
  )
import Unison.Syntax.NameSegment qualified as NameSegment (libSegment, unsafeParseText)

handleInstallLib :: ProjectAndBranch ProjectName (Maybe ProjectBranchNameOrLatestRelease) -> Cli ()
handleInstallLib (ProjectAndBranch libdepProjectName unresolvedLibdepBranchName) = do
  (currentProjectAndBranch, _path) <- ProjectUtils.expectCurrentProjectBranch

  let currentProjectBranchPath =
        ProjectUtils.projectBranchPath $
          ProjectAndBranch
            currentProjectAndBranch.project.projectId
            currentProjectAndBranch.branch.branchId

  libdepProject <- ProjectUtils.expectRemoteProjectByName libdepProjectName

  libdepBranchName <-
    case fromMaybe ProjectBranchNameOrLatestRelease'LatestRelease unresolvedLibdepBranchName of
      ProjectBranchNameOrLatestRelease'Name name -> pure name
      ProjectBranchNameOrLatestRelease'LatestRelease -> ProjectUtils.expectLatestReleaseBranchName libdepProject

  let libdepProjectAndBranchNames =
        ProjectAndBranch libdepProjectName libdepBranchName

  libdepProjectBranch <-
    ProjectUtils.expectRemoteProjectBranchByName
      Share.IncludeSquashedHead
      (ProjectAndBranch (libdepProject.projectId, libdepProjectName) libdepBranchName)

  Cli.Env {codebase} <- ask

  causalHash <-
    downloadProjectBranchFromShare Share.IncludeSquashedHead libdepProjectBranch
      & onLeftM (Cli.returnEarly . Output.ShareError)

  remoteBranchObject <- liftIO (Codebase.expectBranchForHash codebase causalHash)

  -- Find the best available dependency name, starting with the best one (e.g. "unison_base_1_0_0"), and tacking on a
  -- "__2", "__3", etc. suffix.
  --
  -- For example, if the best name is "foo", and libdeps "foo" and "foo__2" already exist, then we'll get "foo__3".
  libdepNameSegment :: NameSegment <- do
    currentBranchObject <- Cli.getBranch0At currentProjectBranchPath
    pure $
      fresh
        (\i -> NameSegment.unsafeParseText . (<> "__" <> tShow i) . NameSegment.toUnescapedText)
        ( case Map.lookup NameSegment.libSegment currentBranchObject._children of
            Nothing -> Set.empty
            Just libdeps -> Map.keysSet (Branch._children (Branch.head libdeps))
        )
        (makeDependencyName libdepProjectName libdepBranchName)

  let libdepPath :: Path.Absolute
      libdepPath =
        Path.resolve
          currentProjectBranchPath
          (Path.Relative (Path.fromList [NameSegment.libSegment, libdepNameSegment]))

  let reflogDescription = "lib.install " <> into @Text libdepProjectAndBranchNames
  _didUpdate <- Cli.updateAt reflogDescription libdepPath (\_empty -> remoteBranchObject)

  Cli.respond (Output.InstalledLibdep libdepProjectAndBranchNames libdepNameSegment)

fresh :: (Ord a) => (Int -> a -> a) -> Set a -> a -> a
fresh bump taken x =
  fromJust (List.find (\y -> not (Set.member y taken)) (x : map (\i -> bump i x) [2 ..]))

-- This function mangles the dependency (a project name + a branch name) to a flat string without special characters,
-- suitable for sticking in the `lib` namespace.
--
-- >>> makeDependencyName (unsafeFrom @Text "@unison/base") (unsafeFrom @Text "main")
-- unison_base_main
--
-- >>> makeDependencyName (unsafeFrom @Text "@unison/base") (unsafeFrom @Text "releases/1.0.0")
-- unison_base_1_0_0
--
-- >>> makeDependencyName (unsafeFrom @Text "@unison/base") (unsafeFrom @Text "releases/drafts/1.0.0")
-- unison_base_1_0_0_draft
--
-- >>> makeDependencyName (unsafeFrom @Text "@unison/base") (unsafeFrom @Text "@person/topic")
-- unison_base_person_topic
makeDependencyName :: ProjectName -> ProjectBranchName -> NameSegment
makeDependencyName projectName branchName =
  NameSegment.unsafeParseText $
    Text.intercalate "_" $
      fold
        [ case projectNameToUserProjectSlugs projectName of
            (user, project) ->
              fold
                [ if Text.null user then [] else [user],
                  [project]
                ],
          case classifyProjectBranchName branchName of
            ProjectBranchNameKind'Contributor user branch -> [user, underscorify branch]
            ProjectBranchNameKind'DraftRelease ver -> semverSegments ver ++ ["draft"]
            ProjectBranchNameKind'Release ver -> semverSegments ver
            ProjectBranchNameKind'NothingSpecial -> [underscorify branchName]
        ]
  where
    semverSegments :: Semver -> [Text]
    semverSegments (Semver x y z) =
      [tShow x, tShow y, tShow z]

    underscorify :: ProjectBranchName -> Text
    underscorify =
      Text.replace "-" "_" . into @Text
