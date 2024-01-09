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
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Witch (unsafeFrom)

handleUpgrade :: NameSegment -> NameSegment -> Cli ()
handleUpgrade oldDepName newDepName = do
  when (oldDepName == newDepName) do
    Cli.returnEarlyWithoutOutput

  Cli.Env {codebase, writeSource} <- ask

  (projectAndBranch, _path) <- Cli.expectCurrentProjectBranch
  let projectId = projectAndBranch ^. #project . #projectId
  let projectPath = Cli.projectBranchPath (ProjectAndBranch projectId (projectAndBranch ^. #branch . #branchId))
  let oldDepPath = Path.resolve projectPath (Path.Relative (Path.fromList [Name.libSegment, oldDepName]))
  let newDepPath = Path.resolve projectPath (Path.Relative (Path.fromList [Name.libSegment, newDepName]))

  currentV1Branch <- Cli.getBranch0At projectPath
  let currentV1BranchWithoutOldDep = deleteLibdep oldDepName currentV1Branch
  oldDep <- Cli.expectBranch0AtPath' oldDepPath
  let oldDepWithoutDeps = deleteLibdeps oldDep
  let oldTransitiveDeps = fromMaybe Branch.empty0 $ fmap Branch.head $ Map.lookup Name.libSegment (oldDep ^. Branch.children)

  newDep <- Cli.expectBranch0AtPath' newDepPath
  let newDepWithoutDeps = deleteLibdeps newDep

  let namesExcludingLibdeps = Branch.toNames (deleteLibdeps currentV1Branch)
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

  let filterTransitiveTerms :: Relation Referent Name -> Set TermReference
      filterTransitiveTerms oldTerms =
        Relation.dom oldTerms
          & Set.mapMaybe \referent -> do
            ref <- Referent.toTermReference referent
            guard (not $ Relation.memberDom referent (Branch.deepTerms currentV1BranchWithoutOldDep))
            pure ref

  let filterTransitiveTypes :: Relation TypeReference Name -> Set TypeReference
      filterTransitiveTypes oldTypes =
        Relation.dom oldTypes
          & Set.filter \typ -> not (Relation.memberDom typ (Branch.deepTypes currentV1BranchWithoutOldDep))

  (unisonFile, printPPE) <-
    Cli.runTransactionWithRollback \abort -> do
      -- Create a Unison file that contains all of our dependents of modified defns of `lib.old`. todo: twiddle
      unisonFile <- do
        dependents <-
          Operations.dependentsWithinScope
            (Names.referenceIds namesExcludingLibdeps)
            ( filterUnchangedTerms (Branch.deepTerms oldDepWithoutDeps)
                <> filterUnchangedTypes (Branch.deepTypes oldDepWithoutDeps)
                <> filterTransitiveTerms (Branch.deepTerms oldTransitiveDeps)
                <> filterTransitiveTypes (Branch.deepTypes oldTransitiveDeps)
            )
        addDefinitionsToUnisonFile
          Output.UOUUpgrade
          abort
          codebase
          namesExcludingLibdeps
          constructorNamesExcludingLibdeps
          dependents
          UnisonFile.emptyUnisonFile
      hashLength <- Codebase.hashLength
      let primaryPPE = makeOldDepPPE oldDepName newDepName namesExcludingOldDep oldDep oldDepWithoutDeps newDepWithoutDeps
      let secondaryPPE = PPED.fromNamesDecl hashLength namesExcludingOldDep
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
      scratchFilePath <-
        Cli.getLatestFile <&> \case
          Nothing -> "scratch.u"
          Just (file, _) -> file
      liftIO $ writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile)
      Cli.respond (Output.UpgradeFailure scratchFilePath oldDepName newDepName)
      Cli.returnEarlyWithoutOutput

  branchUpdates <-
    Cli.runTransactionWithRollback \abort -> do
      Codebase.addDefsToCodebase codebase typecheckedUnisonFile
      typecheckedUnisonFileToBranchUpdates
        abort
        (findCtorNames Output.UOUUpgrade namesExcludingLibdeps constructorNamesExcludingLibdeps Nothing)
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

makeOldDepPPE ::
  NameSegment ->
  NameSegment ->
  Names ->
  Branch0 m ->
  Branch0 m ->
  Branch0 m ->
  PrettyPrintEnvDecl
makeOldDepPPE oldDepName newDepName namesExcludingOldDep oldDep oldDepWithoutDeps newDepWithoutDeps =
  let makePPE suffixifyTerms suffixifyTypes =
        PrettyPrintEnv
          { termNames = \ref ->
              let oldDirectNames = Relation.lookupDom ref (Branch.deepTerms oldDepWithoutDeps)
                  newDirectRefsForOldDirectNames =
                    Relation.range (Branch.deepTerms newDepWithoutDeps) `Map.restrictKeys` oldDirectNames
               in case ( Set.null oldDirectNames,
                         Map.null newDirectRefsForOldDirectNames,
                         Set.member ref (Branch.deepReferents oldDep),
                         Relation.memberRan ref (Names.terms namesExcludingOldDep)
                       ) of
                    (False, False, _, _) ->
                      Names.namesForReferent fakeNames ref
                        & Set.toList
                        & map (\name -> (HQ'.fromName name, HQ'.fromName name))
                        & suffixifyTerms
                        & PPE.Names.prioritize
                    (_, _, True, False) ->
                      Names.namesForReferent prefixedOldNames ref
                        & Set.toList
                        & map (\name -> (HQ'.fromName name, HQ'.fromName name))
                        & PPE.Names.prioritize
                    _ -> [],
            typeNames = \ref ->
              let oldDirectNames = Relation.lookupDom ref (Branch.deepTypes oldDepWithoutDeps)
                  newDirectRefsForOldDirectNames =
                    Relation.range (Branch.deepTypes newDepWithoutDeps) `Map.restrictKeys` oldDirectNames
               in case ( Set.null oldDirectNames,
                         Map.null newDirectRefsForOldDirectNames,
                         Set.member ref (Branch.deepTypeReferences oldDep),
                         Relation.memberRan ref (Names.types namesExcludingOldDep)
                       ) of
                    (False, False, _, _) ->
                      Names.namesForReference fakeNames ref
                        & Set.toList
                        & map (\name -> (HQ'.fromName name, HQ'.fromName name))
                        & suffixifyTypes
                        & PPE.Names.prioritize
                    (_, _, True, False) ->
                      Names.namesForReference prefixedOldNames ref
                        & Set.toList
                        & map (\name -> (HQ'.fromName name, HQ'.fromName name))
                        & PPE.Names.prioritize
                    _ -> []
          }
   in PrettyPrintEnvDecl
        { unsuffixifiedPPE = makePPE id id,
          suffixifiedPPE =
            makePPE
              (PPE.Names.shortestUniqueSuffixes (Names.terms namesExcludingOldDep))
              (PPE.Names.shortestUniqueSuffixes (Names.types namesExcludingOldDep))
        }
  where
    oldNames = Branch.toNames oldDep
    prefixedOldNames = Names.prefix0 (Name.fromReverseSegments (oldDepName :| [Name.libSegment])) oldNames
    fakeNames = Names.prefix0 (Name.fromReverseSegments (newDepName :| [Name.libSegment])) oldNames

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

deleteLibdeps :: Branch0 m -> Branch0 m
deleteLibdeps =
  over Branch.children (Map.delete Name.libSegment)
