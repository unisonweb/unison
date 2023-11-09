module Unison.Codebase.Editor.HandleInput.Upgrade
  ( handleUpgrade,
  )
where

import Control.Lens (ix, over, (^.))
import Control.Monad.Reader (ask)
import Data.List qualified as List
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Sqlite.DbId (ProjectId)
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.HandleInput.Update2 (addDefinitionsToUnisonFile, findCtorNames, forwardCtorNames, prettyParseTypecheck, typecheckedUnisonFileToBranchUpdates)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Prelude
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.Sqlite (Transaction)
import Unison.UnisonFile qualified as UnisonFile
import Witch (unsafeFrom)

handleUpgrade :: NameSegment -> NameSegment -> Cli ()
handleUpgrade oldDepName newDepName = do
  Cli.Env {codebase} <- ask

  (projectAndBranch, _path) <- Cli.expectCurrentProjectBranch
  let projectId = projectAndBranch ^. #project . #projectId
  let projectPath = Cli.projectBranchPath (ProjectAndBranch projectId (projectAndBranch ^. #branch . #branchId))
  let oldDepPath = Path.resolve projectPath (Path.Relative (Path.fromList [Name.libSegment, oldDepName]))
  let newDepPath = Path.resolve projectPath (Path.Relative (Path.fromList [Name.libSegment, newDepName]))

  currentV1Branch <- Cli.getBranch0At projectPath
  let currentV1BranchWithoutOldDep = deleteLibdep oldDepName currentV1Branch
  oldDepV1Branch <- Cli.expectBranch0AtPath' oldDepPath
  _newDepV1Branch <- Cli.expectBranch0AtPath' newDepPath

  let namesExcludingLibdeps = Branch.toNames (currentV1Branch & over Branch.children (Map.delete Name.libSegment))
  let constructorNamesExcludingLibdeps = forwardCtorNames namesExcludingLibdeps
  let namesExcludingOldDep = Branch.toNames currentV1BranchWithoutOldDep

  -- Compute "fake names": these are all of things in `lib.old`, with the `old` segment swapped out for `new`
  let fakeNames =
        oldDepV1Branch
          & Branch.toNames
          & Names.prefix0 (Name.fromReverseSegments (newDepName :| [Name.libSegment]))

  -- Create a Unison file that contains all of our dependents of things in `lib.old`.
  (unisonFile, printPPE) <-
    Cli.runTransaction do
      dependents <-
        Operations.dependentsWithinScope
          (Names.referenceIds namesExcludingLibdeps)
          (Branch.deepTermReferences oldDepV1Branch <> Branch.deepTypeReferences oldDepV1Branch)
      unisonFile <-
        addDefinitionsToUnisonFile
          codebase
          namesExcludingLibdeps
          constructorNamesExcludingLibdeps
          dependents
          UnisonFile.emptyUnisonFile
      -- Construct a PPE to use for rendering the Unison file full of dependents.
      hashLength <- Codebase.hashLength
      let printPPE =
            PPED.fromNamesDecl
              hashLength
              (NamesWithHistory.fromCurrentNames (namesExcludingOldDep <> fakeNames))
      pure (unisonFile, printPPE)

  typecheckedUnisonFile <-
    prettyParseTypecheck unisonFile printPPE & onLeftM \prettyUnisonFile -> do
      -- Small race condition: since picking a branch name and creating the branch happen in different
      -- transactions, creating could fail.
      temporaryBranchName <- Cli.runTransaction (findTemporaryBranchName projectId oldDepName newDepName)
      temporaryBranchId <-
        HandleInput.Branch.doCreateBranch
          (HandleInput.Branch.CreateFrom'Branch projectAndBranch)
          (projectAndBranch ^. #project)
          temporaryBranchName
          textualDescriptionOfUpgrade
      let temporaryBranchPath = Path.unabsolute (Cli.projectBranchPath (ProjectAndBranch projectId temporaryBranchId))
      Cli.stepAt textualDescriptionOfUpgrade (temporaryBranchPath, \_ -> currentV1BranchWithoutOldDep)
      Cli.Env {isTranscript} <- ask
      maybePath <- if isTranscript then pure Nothing else Just . fst <$> Cli.expectLatestFile
      Cli.respond (Output.DisplayDefinitionsString maybePath prettyUnisonFile)
      -- TODO: output message indicating what's going on
      Cli.returnEarlyWithoutOutput

  -- Happy path: save updated things to codebase, cons namespace. Don't forget to delete `lib.old`
  Cli.runTransaction (Codebase.addDefsToCodebase codebase typecheckedUnisonFile)
  Cli.stepAt
    textualDescriptionOfUpgrade
    ( Path.unabsolute projectPath,
      deleteLibdep oldDepName
        . Branch.batchUpdates
          ( typecheckedUnisonFileToBranchUpdates
              (findCtorNames namesExcludingLibdeps constructorNamesExcludingLibdeps Nothing)
              typecheckedUnisonFile
          )
    )
  Cli.respond Output.Success
  where
    textualDescriptionOfUpgrade :: Text
    textualDescriptionOfUpgrade =
      Text.unwords ["upgrade", NameSegment.toText oldDepName, NameSegment.toText newDepName]

-- @findTemporaryBranchName projectId oldDepName newDepName@ finds some unused branch name in @projectId@ with a name
-- like "upgrade-<oldDepName>-to-<newDepName>".
findTemporaryBranchName :: ProjectId -> NameSegment -> NameSegment -> Transaction ProjectBranchName
findTemporaryBranchName projectId oldDepName newDepName = do
  allBranchNames <-
    fmap (Set.fromList . map snd) do
      Queries.loadAllProjectBranchesBeginningWith projectId Nothing

  let -- all branch name candidates in order of preference:
      --   upgrade-<old>-to-<new>
      --   upgrade-<old>-to-<new>-2
      --   upgrade-<old>-to-<new>-3
      --   ...
      allCandidates :: [ProjectBranchName]
      allCandidates =
        preferred : do
          n <- [(2 :: Int) ..]
          pure (unsafeFrom @Text (into @Text preferred <> "-" <> tShow n))
        where
          preferred :: ProjectBranchName
          preferred =
            unsafeFrom @Text $
              "upgrade-"
                <> NameSegment.toText oldDepName
                <> "-to-"
                <> NameSegment.toText newDepName

  pure (fromJust (List.find (\name -> not (Set.member name allBranchNames)) allCandidates))

deleteLibdep :: NameSegment -> Branch0 m -> Branch0 m
deleteLibdep dep =
  over (Branch.children . ix Name.libSegment . Branch.head_ . Branch.children) (Map.delete dep)
