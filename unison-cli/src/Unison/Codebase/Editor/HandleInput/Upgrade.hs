-- | @upgrade@ input handler.
module Unison.Codebase.Editor.HandleInput.Upgrade
  ( handleUpgrade,
  )
where

import Control.Monad.Reader (ask)
import Data.Char qualified as Char
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Builder qualified
import U.Codebase.Sqlite.DbId (ProjectId)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.Branch (CreateFrom (..))
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
import Unison.Codebase.ProjectPath qualified as PP
import Unison.HashQualifiedPrime qualified as HQ'
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
import Unison.Project (ProjectBranchName)
import Unison.Reference (TermReference, TypeReference)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Sqlite (Transaction)
import Unison.Syntax.NameSegment qualified as NameSegment (toEscapedText)
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

  let oldPath = Path.Absolute (Path.fromList [NameSegment.libSegment, oldName])
  let newPath = Path.Absolute (Path.fromList [NameSegment.libSegment, newName])

  currentNamespace <- Cli.getCurrentProjectRoot
  let currentNamespaceSansOld = currentNamespace & Branch.step (Branch.deleteLibdep oldName)
  let currentNamespaceSansOld0 = Branch.head currentNamespaceSansOld
  let currentDeepTermsSansOld = Branch.deepTerms currentNamespaceSansOld0
  let currentDeepTypesSansOld = Branch.deepTypes currentNamespaceSansOld0
  let currentLocalNames = Branch.toNames (Branch.deleteLibdeps $ Branch.head currentNamespace)
  let currentLocalConstructorNames = forwardCtorNames currentLocalNames
  let currentDeepNamesSansOld = Branch.toNames currentNamespaceSansOld0

  oldNamespace <- Cli.expectBranch0AtPath' (Path.AbsolutePath' oldPath)
  let oldLocalNamespace = Branch.deleteLibdeps oldNamespace
  let oldLocalTerms = Branch.deepTerms oldLocalNamespace
  let oldLocalTypes = Branch.deepTypes oldLocalNamespace
  let oldNamespaceMinusLocal = maybe Branch.empty0 Branch.head (Map.lookup NameSegment.libSegment (oldNamespace ^. Branch.children))
  let oldDeepMinusLocalTerms = Branch.deepTerms oldNamespaceMinusLocal
  let oldDeepMinusLocalTypes = Branch.deepTypes oldNamespaceMinusLocal

  newNamespace <- Cli.expectBranch0AtPath' (Path.AbsolutePath' newPath)
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
            (Branch.toNames oldNamespace)
            (Branch.toNames oldLocalNamespace)
            (Branch.toNames newLocalNamespace)
            `PPED.addFallback` makeComplicatedPPE hashLength currentDeepNamesSansOld mempty dependents
        )

  pp@(PP.ProjectPath project projectBranch _path) <- Cli.getCurrentProjectPath
  parsingEnv <- makeParsingEnv pp currentDeepNamesSansOld
  typecheckedUnisonFile <-
    prettyParseTypecheck unisonFile printPPE parsingEnv & onLeftM \prettyUnisonFile -> do
      let getTemporaryBranchName = findTemporaryBranchName (project ^. #projectId) oldName newName
      (_temporaryBranchId, temporaryBranchName) <-
        HandleInput.Branch.createBranch
          textualDescriptionOfUpgrade
          (CreateFrom'NamespaceWithParent projectBranch currentNamespaceSansOld)
          project
          getTemporaryBranchName
      scratchFilePath <-
        Cli.getLatestFile <&> \case
          Nothing -> "scratch.u"
          Just (file, _) -> file
      liftIO $ writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile)
      Cli.returnEarly $
        Output.UpgradeFailure (projectBranch ^. #name) temporaryBranchName scratchFilePath oldName newName

  branchUpdates <-
    Cli.runTransactionWithRollback \abort -> do
      Codebase.addDefsToCodebase codebase typecheckedUnisonFile
      typecheckedUnisonFileToBranchUpdates
        abort
        (findCtorNamesMaybe Output.UOUUpgrade currentLocalNames currentLocalConstructorNames Nothing)
        typecheckedUnisonFile
  Cli.stepAt
    textualDescriptionOfUpgrade
    ( PP.toRoot pp,
      Branch.deleteLibdep oldName . Branch.batchUpdates branchUpdates
    )
  Cli.respond (Output.UpgradeSuccess oldName newName)
  where
    textualDescriptionOfUpgrade :: Text
    textualDescriptionOfUpgrade =
      Text.unwords ["upgrade", NameSegment.toEscapedText oldName, NameSegment.toEscapedText newName]

keepOldLocalTermsNotInNew :: Relation Referent Name -> Relation Referent Name -> Set TermReference
keepOldLocalTermsNotInNew oldLocalTerms newLocalTerms =
  f oldLocalTerms `Set.difference` f newLocalTerms
  where
    f :: Relation Referent Name -> Set TermReference
    f =
      Set.mapMaybe Referent.toTermReference . Relation.dom

keepOldLocalTypesNotInNew :: Relation TypeReference Name -> Relation TypeReference Name -> Set TypeReference
keepOldLocalTypesNotInNew oldLocalTypes newLocalTypes =
  Relation.dom oldLocalTypes `Set.difference` Relation.dom newLocalTypes

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
  Names ->
  Names ->
  Names ->
  PrettyPrintEnvDecl
makeOldDepPPE oldName newName currentDeepNamesSansOld oldDeepNames oldLocalNames newLocalNames =
  let makePPE suffixifier =
        PPE.PrettyPrintEnv termToNames typeToNames
        where
          termToNames :: Referent -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
          termToNames ref
            | inNewNamespace = []
            | hasNewLocalTermsForOldLocalNames = PPE.makeTermNames fakeLocalNames suffixifier ref
            | onlyInOldNamespace = PPE.makeTermNames fullOldDeepNames PPE.dontSuffixify ref
            | otherwise = []
            where
              inNewNamespace = Relation.memberRan ref (Names.terms newLocalNames)
              hasNewLocalTermsForOldLocalNames =
                not (Map.null (Relation.domain (Names.terms newLocalNames) `Map.restrictKeys` theOldLocalNames))
              theOldLocalNames = Relation.lookupRan ref (Names.terms oldLocalNames)
              onlyInOldNamespace = inOldNamespace && not inCurrentNamespaceSansOld
              inOldNamespace = Relation.memberRan ref (Names.terms oldDeepNames)
              inCurrentNamespaceSansOld = Relation.memberRan ref (Names.terms currentDeepNamesSansOld)
          typeToNames :: TypeReference -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
          typeToNames ref
            | inNewNamespace = []
            | hasNewLocalTypesForOldLocalNames = PPE.makeTypeNames fakeLocalNames suffixifier ref
            | onlyInOldNamespace = PPE.makeTypeNames fullOldDeepNames PPE.dontSuffixify ref
            | otherwise = []
            where
              inNewNamespace = Relation.memberRan ref (Names.types newLocalNames)
              hasNewLocalTypesForOldLocalNames =
                not (Map.null (Relation.domain (Names.types newLocalNames) `Map.restrictKeys` theOldLocalNames))
              theOldLocalNames = Relation.lookupRan ref (Names.types oldLocalNames)
              onlyInOldNamespace = inOldNamespace && not inCurrentNamespaceSansOld
              inOldNamespace = Relation.memberRan ref (Names.types oldDeepNames)
              inCurrentNamespaceSansOld = Relation.memberRan ref (Names.types currentDeepNamesSansOld)
   in PrettyPrintEnvDecl
        { unsuffixifiedPPE = makePPE PPE.dontSuffixify,
          suffixifiedPPE = makePPE (PPE.suffixifyByHash currentDeepNamesSansOld)
        }
  where
    -- "full" means "with lib.old.* prefix"
    fullOldDeepNames = PPE.namer (Names.prefix0 (Name.fromReverseSegments (oldName :| [NameSegment.libSegment])) oldDeepNames)
    fakeLocalNames = PPE.namer (Names.prefix0 (Name.fromReverseSegments (newName :| [NameSegment.libSegment])) oldLocalNames)

-- @findTemporaryBranchName projectId oldDepName newDepName@ finds some unused branch name in @projectId@ with a name
-- like "upgrade-<oldDepName>-to-<newDepName>".
findTemporaryBranchName :: ProjectId -> NameSegment -> NameSegment -> Transaction ProjectBranchName
findTemporaryBranchName projectId oldDepName newDepName = do
  Cli.findTemporaryBranchName projectId $
    -- First try something like
    --
    --   upgrade-unison_base_3_0_0-to-unison_base_4_0_0
    --
    -- and if that fails (which it shouldn't, but may because of symbols or something), back off to some
    -- more-guaranteed-to-work mangled name like
    --
    --   upgrade-unisonbase300-to-unisonbase400
    tryFrom @Text (mk oldDepText newDepText)
      & fromRight (unsafeFrom @Text (mk (scrub oldDepText) (scrub newDepText)))
  where
    mk :: Text -> Text -> Text
    mk old new =
      Text.Builder.run ("upgrade-" <> Text.Builder.text old <> "-to-" <> Text.Builder.text new)

    scrub :: Text -> Text
    scrub =
      Text.filter Char.isAlphaNum

    oldDepText = NameSegment.toEscapedText oldDepName
    newDepText = NameSegment.toEscapedText newDepName
