-- | @lib.install@ input handler
module Unison.Codebase.Editor.HandleInput.InstallLib
  ( handleInstallLib,
    handleInstallLocalLib,
  )
where

import Control.Monad.Reader (ask)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import U.Codebase.Causal qualified as UCausal
import U.Codebase.Causal.Squash qualified as UCausal
import U.Codebase.Sqlite.V2.HashHandle qualified as HH
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
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Core.Project (ProjectBranchName)
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment (libSegment)
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
import Unison.Syntax.NameSegment qualified as NameSegment

handleInstallLib :: Bool -> ProjectAndBranch ProjectName (Maybe ProjectBranchNameOrLatestRelease) -> Cli ()
handleInstallLib remind (ProjectAndBranch libdepProjectName unresolvedLibdepBranchName) = do
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

  when remind do
    Cli.respond (Output.UseLibInstallNotPull (ProjectAndBranch libdepProjectName libdepBranchName))

  Cli.Env {codebase} <- ask

  causalHash <-
    downloadProjectBranchFromShare Share.IncludeSquashedHead libdepProjectBranch
      & onLeftM (Cli.returnEarly . Output.ShareError)

  remoteBranchObject <- liftIO (Codebase.expectBranchForHash codebase causalHash)
  let reflogDescription = "lib.install " <> into @Text libdepProjectAndBranchNames
  libdepNameSegment <- attachNewLib reflogDescription remoteBranchObject libdepProjectName libdepBranchName Nothing
  Cli.respond (Output.InstalledLibdep libdepProjectAndBranchNames libdepNameSegment)

-- | Attach a new library to the current project branch, under the `lib` namespace, using a fresh name derived from the
-- project and branch names.
attachNewLib :: Text -> Branch.Branch IO -> ProjectName -> ProjectBranchName -> Maybe NameSegment -> Cli NameSegment
attachNewLib reflogDescription libBranch libdepProjectName libdepBranchName preferredName = do
  -- Find the best available dependency name, starting with the best one (e.g. "unison_base_1_0_0"), and tacking on a
  -- "__2", "__3", etc. suffix.
  --
  -- For example, if the best name is "foo", and libdeps "foo" and "foo__2" already exist, then we'll get "foo__3".
  libdepNameSegment :: NameSegment <- case preferredName of
    Just name -> pure name
    Nothing -> do
      currentBranchObject <- Cli.getCurrentProjectRoot0
      pure $
        fresh
          (\i -> NameSegment.unsafeParseText . (<> "__" <> tShow i) . NameSegment.toUnescapedText)
          ( case Map.lookup NameSegment.libSegment (currentBranchObject ^. Branch.children_) of
              Nothing -> Set.empty
              Just libdeps -> Map.keysSet (Branch.head libdeps ^. Branch.children_)
          )
          (makeDependencyName libdepProjectName libdepBranchName)

  let libdepPath :: Path.Absolute
      libdepPath = Path.Absolute $ Path.fromList [NameSegment.libSegment, libdepNameSegment]

  pp <- Cli.getCurrentProjectPath
  let libDepPP = pp & PP.absPath_ .~ libdepPath
  _didUpdate <- Cli.updateAt reflogDescription libDepPP (\_empty -> libBranch)
  pure libdepNameSegment

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
    Text.replace "-" "_" $
      Text.intercalate "_" $
        fold
          [ case projectNameToUserProjectSlugs projectName of
              (user, project) ->
                fold
                  [ if Text.null user then [] else [user],
                    [project]
                  ],
            case classifyProjectBranchName branchName of
              ProjectBranchNameKind'Contributor user branch -> [user, into @Text branch]
              ProjectBranchNameKind'DraftRelease ver -> semverSegments ver ++ ["draft"]
              ProjectBranchNameKind'Release ver -> semverSegments ver
              ProjectBranchNameKind'NothingSpecial -> [into @Text branchName]
          ]
  where
    semverSegments :: Semver -> [Text]
    semverSegments (Semver x y z) =
      [tShow x, tShow y, tShow z]

---------------------------------

handleInstallLocalLib :: (ProjectAndBranch ProjectName ProjectBranchName) -> Maybe NameSegment -> Cli ()
handleInstallLocalLib srcPAB@(ProjectAndBranch projName branchName) mayDestLibName = do
  sourcePAB <- ProjectUtils.expectProjectAndBranchByTheseNames (These projName branchName)
  causalBranchToSquash <- Cli.runTransaction $ Codebase.expectProjectBranchRootCausal sourcePAB.branch
  squashResult <- Cli.runTransaction $ UCausal.squashCausal HH.v2HashHandle causalBranchToSquash
  let reflogDescription = "lib.install " <> into @Text srcPAB <> maybe "" (\n -> " " <> NameSegment.toEscapedText n) mayDestLibName
  Cli.Env {codebase} <- ask
  squashedBranchIO <- liftIO $ Codebase.expectBranchForHash codebase squashResult.causalHash
  libSegmentName <- attachNewLib reflogDescription squashedBranchIO projName branchName mayDestLibName
  Cli.respond $ Output.InstalledLibdep srcPAB libSegmentName
