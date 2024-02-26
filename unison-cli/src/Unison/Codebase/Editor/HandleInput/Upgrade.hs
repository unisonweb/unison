module Unison.Codebase.Editor.HandleInput.Upgrade
  ( handleUpgrade,
  )
where

import Control.Lens ((^.))
import Control.Monad.Reader (ask)
import Data.List qualified as List
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Set qualified as Set
import Data.Text qualified as Text
import U.Codebase.Sqlite.DbId (ProjectId)
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
    findCtorNamesMaybe,
    forwardCtorNames,
    getNamespaceDependentsOf,
    makeComplicatedPPE,
    makeParsingEnv,
    prettyParseTypecheck,
    typecheckedUnisonFileToBranchUpdates,
  )
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names (..))
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import Unison.PrettyPrintEnvDecl qualified as PPED (addFallback)
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
handleUpgrade oldName newName = do
  when (oldName == newName) do
    Cli.returnEarlyWithoutOutput

  Cli.Env {codebase, writeSource} <- ask

  (projectAndBranch, _path) <- Cli.expectCurrentProjectBranch
  let projectId = projectAndBranch ^. #project . #projectId
  let projectPath = Cli.projectBranchPath (ProjectAndBranch projectId (projectAndBranch ^. #branch . #branchId))
  let oldPath = Path.resolve projectPath (Path.Relative (Path.fromList [Name.libSegment, oldName]))
  let newPath = Path.resolve projectPath (Path.Relative (Path.fromList [Name.libSegment, newName]))

  currentNamespace <- Cli.getBranch0At projectPath
  let currentNamespaceSansOld = Branch.deleteLibdep oldName currentNamespace
  let currentDeepTermsSansOld = Branch.deepTerms currentNamespaceSansOld
  let currentDeepTypesSansOld = Branch.deepTypes currentNamespaceSansOld
  let currentLocalNames = Branch.toNames (Branch.deleteLibdeps currentNamespace)
  let currentLocalConstructorNames = forwardCtorNames currentLocalNames
  let currentDeepNamesSansOld = Branch.toNames currentNamespaceSansOld

  oldNamespace <- Cli.expectBranch0AtPath' oldPath
  let oldLocalNamespace = Branch.deleteLibdeps oldNamespace
  let oldLocalTerms = Branch.deepTerms oldLocalNamespace
  let oldLocalTypes = Branch.deepTypes oldLocalNamespace
  let oldNamespaceMinusLocal = maybe Branch.empty0 Branch.head (Map.lookup Name.libSegment (oldNamespace ^. Branch.children))
  let oldDeepMinusLocalTerms = Branch.deepTerms oldNamespaceMinusLocal
  let oldDeepMinusLocalTypes = Branch.deepTypes oldNamespaceMinusLocal

  newNamespace <- Cli.expectBranch0AtPath' newPath
  let newLocalNamespace = Branch.deleteLibdeps newNamespace
  let newLocalTerms = Branch.deepTerms newLocalNamespace
  let newLocalTypes = Branch.deepTypes newLocalNamespace

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
    Cli.runTransactionWithRollback \abort -> do
      dependents <-
        getNamespaceDependentsOf
          currentLocalNames
          ( Set.unions
              [ keepOldLocalTermsNotInNew oldLocalTerms newLocalTerms,
                keepOldLocalTypesNotInNew oldLocalTypes newLocalTypes,
                keepOldDeepTermsStillInUse oldDeepMinusLocalTerms currentDeepTermsSansOld,
                keepOldDeepTypesStillInUse oldDeepMinusLocalTypes currentDeepTypesSansOld
              ]
          )
      unisonFile <- do
        addDefinitionsToUnisonFile
          abort
          codebase
          (findCtorNames Output.UOUUpgrade currentLocalNames currentLocalConstructorNames)
          dependents
          UnisonFile.emptyUnisonFile
      hashLength <- Codebase.hashLength
      pure
        ( unisonFile,
          makeOldDepPPE
            oldName
            newName
            currentDeepNamesSansOld
            oldNamespace
            oldLocalTerms
            oldLocalTypes
            newLocalTerms
            newLocalTypes
            `PPED.addFallback` makeComplicatedPPE hashLength currentDeepNamesSansOld mempty dependents
        )

  parsingEnv <- makeParsingEnv projectPath currentDeepNamesSansOld
  typecheckedUnisonFile <-
    prettyParseTypecheck unisonFile printPPE parsingEnv & onLeftM \prettyUnisonFile -> do
      -- Small race condition: since picking a branch name and creating the branch happen in different
      -- transactions, creating could fail.
      temporaryBranchName <- Cli.runTransaction (findTemporaryBranchName projectId oldName newName)
      temporaryBranchId <-
        HandleInput.Branch.doCreateBranch
          (HandleInput.Branch.CreateFrom'Branch projectAndBranch)
          (projectAndBranch ^. #project)
          temporaryBranchName
          textualDescriptionOfUpgrade
      let temporaryBranchPath = Path.unabsolute (Cli.projectBranchPath (ProjectAndBranch projectId temporaryBranchId))
      Cli.stepAt textualDescriptionOfUpgrade (temporaryBranchPath, \_ -> currentNamespaceSansOld)
      scratchFilePath <-
        Cli.getLatestFile <&> \case
          Nothing -> "scratch.u"
          Just (file, _) -> file
      liftIO $ writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile)
      Cli.respond (Output.UpgradeFailure scratchFilePath oldName newName)
      Cli.returnEarlyWithoutOutput

  branchUpdates <-
    Cli.runTransactionWithRollback \abort -> do
      Codebase.addDefsToCodebase codebase typecheckedUnisonFile
      typecheckedUnisonFileToBranchUpdates
        abort
        (findCtorNamesMaybe Output.UOUUpgrade currentLocalNames currentLocalConstructorNames Nothing)
        typecheckedUnisonFile
  Cli.stepAt
    textualDescriptionOfUpgrade
    ( Path.unabsolute projectPath,
      Branch.deleteLibdep oldName . Branch.batchUpdates branchUpdates
    )
  Cli.respond (Output.UpgradeSuccess oldName newName)
  where
    textualDescriptionOfUpgrade :: Text
    textualDescriptionOfUpgrade =
      Text.unwords ["upgrade", NameSegment.toText oldName, NameSegment.toText newName]

-- Keep only the old terms that aren't "in" new, where "in" is defined as follows:
--
--   * Consider some term in old, #foo, with set of names { "bar", "baz" }.
--
--   * We say this term is "in" new if the names associated with #foo include at least "bar" or "baz" (that is, there is
--   a non-empty intersection of sets of names).
--
-- Here are a couple common cases:
--
--   1. A term #foo isn't touched between old and new versions, i.e. it has the same set of names in both. This function
--      would not return such a term.
--
--   2. A term #old => { "foo" } exists in old, but not in new, because it's been updated to #new => { "foo" }. This
--      function would return #old.
keepOldLocalTermsNotInNew :: Relation Referent Name -> Relation Referent Name -> Set TermReference
keepOldLocalTermsNotInNew oldLocalTerms newLocalTerms =
  Map.foldMapWithKey phi (Relation.domain oldLocalTerms)
  where
    phi :: Referent -> Set Name -> Set TermReference
    phi referent oldNames =
      case Referent.toTermReference referent of
        Nothing -> Set.empty
        Just ref ->
          let newNames = Relation.lookupDom referent newLocalTerms
           in case newNames `Set.disjoint` oldNames of
                True -> Set.singleton ref
                False -> Set.empty

keepOldLocalTypesNotInNew :: Relation TypeReference Name -> Relation TypeReference Name -> Set TypeReference
keepOldLocalTypesNotInNew oldLocalTypes newLocalTypes =
  Map.foldMapWithKey phi (Relation.domain oldLocalTypes)
  where
    phi :: TypeReference -> Set Name -> Set TypeReference
    phi typeRef oldNames =
      let newNames = Relation.lookupDom typeRef newLocalTypes
       in case newNames `Set.disjoint` oldNames of
            True -> Set.singleton typeRef
            False -> Set.empty

keepOldDeepTermsStillInUse :: Relation Referent Name -> Relation Referent Name -> Set TermReference
keepOldDeepTermsStillInUse oldDeepMinusLocalTerms currentDeepTermsSansOld =
  Relation.dom oldDeepMinusLocalTerms & Set.mapMaybe \referent -> do
    ref <- Referent.toTermReference referent
    guard (not (Relation.memberDom referent currentDeepTermsSansOld))
    pure ref

keepOldDeepTypesStillInUse :: Relation TypeReference Name -> Relation TypeReference Name -> Set TypeReference
keepOldDeepTypesStillInUse oldDeepMinusLocalTypes currentDeepTypesSansOld =
  Relation.dom oldDeepMinusLocalTypes
    & Set.filter \typ -> not (Relation.memberDom typ currentDeepTypesSansOld)

makeOldDepPPE ::
  NameSegment ->
  NameSegment ->
  Names ->
  Branch0 m ->
  Relation Referent Name ->
  Relation TypeReference Name ->
  Relation Referent Name ->
  Relation TypeReference Name ->
  PrettyPrintEnvDecl
makeOldDepPPE oldName newName currentDeepNamesSansOld oldNamespace oldLocalTerms oldLocalTypes newLocalTerms newLocalTypes =
  let makePPE suffixifier =
        PPE.PrettyPrintEnv termToNames typeToNames
        where
          termToNames ref =
            case ( Set.null oldDirectNames,
                   Map.null newDirectRefsForOldDirectNames,
                   Set.member ref (Branch.deepReferents oldNamespace),
                   Relation.memberRan ref (Names.terms currentDeepNamesSansOld)
                 ) of
              (False, False, _, _) -> PPE.makeTermNames fakeNames suffixifier ref
              (_, _, True, False) -> PPE.makeTermNames prefixedOldNames PPE.dontSuffixify ref
              _ -> []
            where
              oldDirectNames = Relation.lookupDom ref oldLocalTerms
              newDirectRefsForOldDirectNames = Relation.range newLocalTerms `Map.restrictKeys` oldDirectNames
          typeToNames ref =
            case ( Set.null oldDirectNames,
                   Map.null newDirectRefsForOldDirectNames,
                   Set.member ref (Branch.deepTypeReferences oldNamespace),
                   Relation.memberRan ref (Names.types currentDeepNamesSansOld)
                 ) of
              (False, False, _, _) -> PPE.makeTypeNames fakeNames suffixifier ref
              (_, _, True, False) -> PPE.makeTypeNames prefixedOldNames PPE.dontSuffixify ref
              _ -> []
            where
              oldDirectNames = Relation.lookupDom ref oldLocalTypes
              newDirectRefsForOldDirectNames = Relation.range newLocalTypes `Map.restrictKeys` oldDirectNames
   in PrettyPrintEnvDecl
        { unsuffixifiedPPE = makePPE PPE.dontSuffixify,
          suffixifiedPPE = makePPE (PPE.suffixifyByHash currentDeepNamesSansOld)
        }
  where
    oldDepNames = Branch.toNames oldNamespace
    prefixedOldNames = PPE.namer (Names.prefix0 (Name.fromReverseSegments (oldName :| [Name.libSegment])) oldDepNames)
    fakeNames = PPE.namer (Names.prefix0 (Name.fromReverseSegments (newName :| [Name.libSegment])) oldDepNames)

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
