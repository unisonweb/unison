module Unison.Codebase.Editor.HandleInput.Upgrade
  ( handleUpgrade,
  )
where

import Control.Lens (ix, over, (^.))
import Control.Monad.Reader (ask)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as Cli
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.HandleInput.Update2 (addDefinitionsToUnisonFile)
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl)
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.UnisonFile (UnisonFile)
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
  oldDepV1Branch <- Cli.expectBranch0AtPath' oldDepPath
  newDepV1Branch <- Cli.expectBranch0AtPath' newDepPath

  let namesIncludingLibdeps = Branch.toNames currentV1Branch
  let namesExcludingLibdeps = Branch.toNames (currentV1Branch & over Branch.children (Map.delete Name.libSegment))
  let namesExcludingOldDep = wundefined

  -- Compute "fake names": these are all of things in `lib.old`, with the `old` segment swapped out for `new`
  let fakeNames :: Names = wundefined
        where
          -- rename "lib.old.X" to "lib.new.X"
          rename :: Name -> Name
          rename = wundefined

  -- Create a Unison file that contains all of our dependents of things in `lib.old`.
  (unisonFile, printPPE) <-
    Cli.runTransaction do
      dependents <-
        Operations.dependentsWithinScope
          (Names.referenceIds namesExcludingLibdeps)
          (Branch.deepTermReferences oldDepV1Branch <> Branch.deepTypeReferences newDepV1Branch)
      unisonFile <- addDefinitionsToUnisonFile codebase namesExcludingLibdeps dependents UnisonFile.emptyUnisonFile
      -- Construct a PPE to use for rendering the Unison file full of dependents.
      printPPE :: PrettyPrintEnvDecl <- wundefined (namesExcludingOldDep <> fakeNames)
      pure (unisonFile, printPPE)

  -- Construct a PPE to use for rendering the Unison file full of dependents.
  let printPPE :: PrettyPrintEnvDecl
      printPPE =
        wundefined (namesExcludingOldDep <> fakeNames)

  -- Round-trip that bad boy through a bad String
  wundefined

  -- Happy path: save updated things to codebase, cons namespace. Don't forget to delete `lib.old`
  wundefined

  -- Sad path:
  --   [x] Make a new project branch, stepped forward one causal (tossing `lib.old`).
  --   [ ] Put the busted dependents into scratch.u
  --   [ ] Output message or something.
  let sadPath = False
  when sadPath do
    temporaryBranchName <- do
      -- Small race condition: since picking a branch name and creating the branch happen in different
      -- transactions, creating could fail.
      allBranchNames <-
        fmap (Set.fromList . map snd) do
          Cli.runTransaction (Queries.loadAllProjectBranchesBeginningWith projectId Nothing)

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

    temporaryBranchId <-
      HandleInput.Branch.doCreateBranch
        (HandleInput.Branch.CreateFrom'Branch projectAndBranch)
        (projectAndBranch ^. #project)
        temporaryBranchName
        textualDescriptionOfUpgrade

    let temporaryBranchPath :: Path
        temporaryBranchPath =
          Path.unabsolute (Cli.projectBranchPath (ProjectAndBranch projectId temporaryBranchId))

    Cli.stepAt textualDescriptionOfUpgrade (temporaryBranchPath, deleteLibdep oldDepName)
  where
    textualDescriptionOfUpgrade :: Text
    textualDescriptionOfUpgrade =
      Text.unwords ["upgrade", NameSegment.toText oldDepName, NameSegment.toText newDepName]

deleteLibdep :: NameSegment -> Branch0 m -> Branch0 m
deleteLibdep dep =
  over (Branch.children . ix Name.libSegment . Branch.head_ . Branch.children) (Map.delete dep)
