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
import Unison.Codebase.Editor.HandleInput.Update2
  ( addDefinitionsToUnisonFile,
    findCtorNames,
    forwardCtorNames,
    makeParsingEnv,
    prettyParseTypecheck,
    typecheckedUnisonFileToBranchUpdates,
  )
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Prelude
import Unison.PrettyPrintEnvDecl qualified as PPED (addFallback)
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.Sqlite (Transaction)
import Unison.UnisonFile qualified as UnisonFile
import Witch (unsafeFrom)

handleUpgrade :: NameSegment -> NameSegment -> Cli ()
handleUpgrade oldDepName newDepName = do
  when (oldDepName == newDepName) do
    Cli.returnEarlyWithoutOutput

  Cli.Env {codebase} <- ask

  (projectAndBranch, _path) <- Cli.expectCurrentProjectBranch
  let projectId = projectAndBranch ^. #project . #projectId
  let projectPath = Cli.projectBranchPath (ProjectAndBranch projectId (projectAndBranch ^. #branch . #branchId))
  let oldDepPath = Path.resolve projectPath (Path.Relative (Path.fromList [Name.libSegment, oldDepName]))
  let newDepPath = Path.resolve projectPath (Path.Relative (Path.fromList [Name.libSegment, newDepName]))

  currentV1Branch <- Cli.getBranch0At projectPath
  let currentV1BranchWithoutOldDep = deleteLibdep oldDepName currentV1Branch
  let currentV1BranchWithoutOldAndNewDeps = deleteLibdep newDepName currentV1BranchWithoutOldDep
  oldDepV1Branch <- Cli.expectBranch0AtPath' oldDepPath
  _newDepV1Branch <- Cli.expectBranch0AtPath' newDepPath

  let namesExcludingLibdeps = Branch.toNames (currentV1Branch & over Branch.children (Map.delete Name.libSegment))
  let constructorNamesExcludingLibdeps = forwardCtorNames namesExcludingLibdeps
  let namesExcludingOldDep = Branch.toNames currentV1BranchWithoutOldDep
  let namesExcludingOldAndNewDeps = Branch.toNames currentV1BranchWithoutOldAndNewDeps

  -- High-level idea: we are trying to perform substitution in every term that depends on something in `old` with the
  -- corresponding thing in `new`, by first rendering the user's code with a particular pretty-print environment, then
  -- parsing it back in a particular parsing environment.
  --
  -- For example, if a user with the namespace
  --
  --     lib.old.foo#oldfoo = 17
  --     lib.new.foo#newfoo = 18
  --     mything#mything    = #oldfoo + 10
  --
  -- runs `upgrade old new`, we will first render
  --
  --     mything#mything    = #oldfoo + 10
  --
  -- as
  --
  --     mything = foo + 10
  --
  -- (note, "foo" here is the shortest unambiguous suffix of all names minus those in `old`), then parse it back in the
  -- parsing environment with names
  --
  --     lib.new.foo = #newfoo
  --
  -- resulting in
  --
  --     mything#mything2 = #newfoo + 10

  (unisonFile, printPPE) <-
    Cli.runTransaction do
      -- Create a Unison file that contains all of our dependents of things in `lib.old`.
      unisonFile <- do
        dependents <-
          Operations.dependentsWithinScope
            (Names.referenceIds namesExcludingLibdeps)
            (Branch.deepTermReferences oldDepV1Branch <> Branch.deepTypeReferences oldDepV1Branch)
        addDefinitionsToUnisonFile
          codebase
          namesExcludingLibdeps
          constructorNamesExcludingLibdeps
          dependents
          UnisonFile.emptyUnisonFile
      hashLength <- Codebase.hashLength
      let namesToPPED = PPED.fromNamesDecl hashLength . NamesWithHistory.fromCurrentNames
      let printPPE1 = namesToPPED namesExcludingOldDep
      -- These "fake names" are all of things in `lib.old`, with the `old` segment swapped out for `new`
      --
      -- If we fall back to this second PPE, we know we have a reference in `fakeNames` (i.e. a reference originally
      -- from `old`, and found nowhere else in our project+libdeps), but we have to include
      -- `namesExcludingOldAndNewDeps` as well, so that we don't over-suffixify.
      --
      -- For example, consider the names
      --
      --   #old = lib.new.foobaloo
      --   #thing = my.project.foobaloo
      --
      -- Were we to fall back on this PPE looking up a name for #old, we'd not want to return "foobaloo", but rather
      -- "new.foobaloo".
      let fakeNames =
            oldDepV1Branch
              & Branch.toNames
              & Names.prefix0 (Name.fromReverseSegments (newDepName :| [Name.libSegment]))
      let printPPE2 = namesToPPED (namesExcludingOldAndNewDeps <> fakeNames)
      pure (unisonFile, printPPE1 `PPED.addFallback` printPPE2)

  parsingEnv <- makeParsingEnv projectPath namesExcludingOldDep
  typecheckedUnisonFile <-
    prettyParseTypecheck unisonFile printPPE parsingEnv & onLeftM \prettyUnisonFile -> do
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
      Cli.respond (Output.UpgradeFailure oldDepName newDepName)
      Cli.returnEarlyWithoutOutput

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
  Cli.respond (Output.UpgradeSuccess oldDepName newDepName)
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
