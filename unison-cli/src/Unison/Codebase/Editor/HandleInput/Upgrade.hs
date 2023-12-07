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
import Unison.HashQualified' qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names (..))
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnv.Names qualified as PPE.Names
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import Unison.PrettyPrintEnvDecl qualified as PPED (addFallback)
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.Reference (TermReference, TypeReference)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Sqlite (Transaction)
import Unison.UnisonFile qualified as UnisonFile
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
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
  oldDepWithoutItsDeps <- over Branch.children (Map.delete Name.libSegment) <$> Cli.expectBranch0AtPath' oldDepPath

  newDepV1Branch <- Cli.expectBranch0AtPath' newDepPath

  let namesExcludingLibdeps = Branch.toNames (currentV1Branch & over Branch.children (Map.delete Name.libSegment))
  let constructorNamesExcludingLibdeps = forwardCtorNames namesExcludingLibdeps
  let namesExcludingOldDep = Branch.toNames currentV1BranchWithoutOldDep

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

  let newDepWithoutDeps = over Branch.children (Map.delete Name.libSegment) newDepV1Branch
  let filterUnchangedTerms :: Relation Referent Name -> Set TermReference
      filterUnchangedTerms oldTerms =
        let phi ref oldNames = case Referent.toTermReference ref of
              Nothing -> Set.empty
              Just termRef ->
                let newNames = Relation.lookupDom ref newTerms
                 in case newNames `Set.disjoint` oldNames of
                      True -> Set.singleton termRef
                      False -> Set.empty
         in Map.foldMapWithKey phi $
              Relation.domain oldTerms
        where
          newTerms = Branch.deepTerms newDepWithoutDeps

  let filterUnchangedTypes :: Relation TypeReference Name -> Set TypeReference
      filterUnchangedTypes oldTypes =
        let phi typeRef oldNames =
              let newNames = Relation.lookupDom typeRef newTypes
               in case newNames `Set.disjoint` oldNames of
                    True -> Set.singleton typeRef
                    False -> Set.empty
         in Map.foldMapWithKey phi $
              Relation.domain oldTypes
        where
          newTypes = Branch.deepTypes newDepWithoutDeps

  (unisonFile, printPPE) <-
    Cli.runTransactionWithRollback \abort -> do
      -- Create a Unison file that contains all of our dependents of modified defns of `lib.old`. todo: twiddle
      unisonFile <- do
        dependents <-
          Operations.dependentsWithinScope
            (Names.referenceIds namesExcludingLibdeps)
            ( filterUnchangedTerms (Branch.deepTerms oldDepWithoutItsDeps)
                <> filterUnchangedTypes (Branch.deepTypes oldDepWithoutItsDeps)
            )
        addDefinitionsToUnisonFile
          abort
          codebase
          namesExcludingLibdeps
          constructorNamesExcludingLibdeps
          dependents
          UnisonFile.emptyUnisonFile
      hashLength <- Codebase.hashLength
      let primaryPPE = makeOldDepPPE newDepName namesExcludingOldDep oldDepWithoutItsDeps
      let secondaryPPE = PPED.fromNamesDecl hashLength (NamesWithHistory.fromCurrentNames namesExcludingOldDep)
      pure (unisonFile, primaryPPE `PPED.addFallback` secondaryPPE)

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

  branchUpdates <-
    Cli.runTransactionWithRollback \abort -> do
      Codebase.addDefsToCodebase codebase typecheckedUnisonFile
      typecheckedUnisonFileToBranchUpdates
        abort
        (findCtorNames namesExcludingLibdeps constructorNamesExcludingLibdeps Nothing)
        typecheckedUnisonFile
  Cli.stepAt
    textualDescriptionOfUpgrade
    ( Path.unabsolute projectPath,
      deleteLibdep oldDepName . Branch.batchUpdates branchUpdates
    )
  Cli.respond (Output.UpgradeSuccess oldDepName newDepName)
  where
    textualDescriptionOfUpgrade :: Text
    textualDescriptionOfUpgrade =
      Text.unwords ["upgrade", NameSegment.toText oldDepName, NameSegment.toText newDepName]

-- `makeOldDepPPE newDepName namesExcludingOldDep oldDepBranch` makes a PPE(D) that only knows how to render `old` deps;
-- other names should be provided by some fallback PPE.
--
-- How we render `old` deps is rather subtle and complicated, but the basic idea is that an `upgrade old new` ought to
-- render all of the old things like `lib.old.foo#oldfoo` as `lib.new.foo` to be parsed and typechecked.
--
-- To render some reference #foo, if it's not a reference that's directly part of old's API (i.e. it has some name in
-- `lib.old.*` that isn't in one of old's deps `lib.old.lib.*`, then return the empty list of names. (Again, the
-- fallback PPE will ultimately provide a name for such a #foo).
--
-- Otherwise, we have some #foo that has at least one name in `lib.old.*`; say it's called `lib.old.foo`. The goal is to
-- render this as `lib.new.foo`, regardless of how many other aliases #foo has in the namespace. (It may be the case
-- that #foo has a name outside of the libdeps, like `my.name.for.foo`, or maybe it has a name in another dependency
-- entirely, like `lib.otherdep.othername`).
makeOldDepPPE :: NameSegment -> Names -> Branch0 m -> PrettyPrintEnvDecl
makeOldDepPPE newDepName namesExcludingOldDep oldDepBranch =
  let makePPE suffixifyTerms suffixifyTypes =
        PrettyPrintEnv
          { termNames = \ref ->
              if Set.member ref termsDirectlyInOldDep
                then -- Say ref is #oldfoo, with two names in `old`:
                --
                --   [ lib.old.foo, lib.old.fooalias ]
                --
                -- We start from that same list of names with `new` swapped in for `old`:
                --
                --   [ lib.new.foo, lib.new.fooalias ]

                  Names.namesForReferent fakeNames ref
                    & Set.toList
                    -- We manually lift those to hashless hash-qualified names, which isn't a very significant
                    -- implementation detail, we just happen to not want hashes, even if the old name like "lib.old.foo"
                    -- was conflicted in `old`.
                    & map (\name -> (HQ'.fromName name, HQ'.fromName name))
                    -- We find the shortest unique suffix of each name in a naming context which:
                    --
                    --   1. Starts from all names, minus the entire `lib.old` namespace.
                    --
                    --   2. Deletes every name for references directly in `lib.old` (i.e. in `lib.old.*` without having
                    --      to descend into some `lib.old.lib.*`.
                    --
                    --      For example, if there's both
                    --
                    --        lib.old.foo#oldfoo
                    --        someAlias#oldfoo
                    --
                    --      then (again, because #oldfoo has a name directly in `lib.old`), we delete names like
                    --      `someAlias#oldfoo`.
                    --
                    --   3. Adds back in names like `lib.new.*` for every hash directly referenced in `lib.old.*`, which
                    --      would be
                    --
                    --        [ lib.new.foo#oldfoo, lib.new.fooalias#oldfoo ]
                    & suffixifyTerms
                    & PPE.Names.prioritize
                else [],
            typeNames = \ref ->
              if Set.member ref typesDirectlyInOldDep
                then
                  Names.namesForReference fakeNames ref
                    & Set.toList
                    & map (\name -> (HQ'.fromName name, HQ'.fromName name))
                    & suffixifyTypes
                    & PPE.Names.prioritize
                else []
          }
   in PrettyPrintEnvDecl
        { unsuffixifiedPPE = makePPE id id,
          suffixifiedPPE =
            makePPE
              ( PPE.Names.shortestUniqueSuffixes $
                  namesExcludingOldDep
                    & Names.terms
                    & Relation.subtractRan termsDirectlyInOldDep
                    & Relation.union (Names.terms fakeNames)
              )
              ( PPE.Names.shortestUniqueSuffixes $
                  namesExcludingOldDep
                    & Names.types
                    & Relation.subtractRan typesDirectlyInOldDep
                    & Relation.union (Names.types fakeNames)
              )
        }
  where
    oldDepWithoutItsDeps = over Branch.children (Map.delete Name.libSegment) oldDepBranch
    termsDirectlyInOldDep = Branch.deepReferents oldDepWithoutItsDeps
    typesDirectlyInOldDep = Branch.deepTypeReferences oldDepWithoutItsDeps
    fakeNames =
      oldDepWithoutItsDeps
        & Branch.toNames
        & Names.prefix0 (Name.fromReverseSegments (newDepName :| [Name.libSegment]))

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
