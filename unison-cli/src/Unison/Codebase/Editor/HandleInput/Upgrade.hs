-- | @upgrade@ input handler.
module Unison.Codebase.Editor.HandleInput.Upgrade
  ( handleUpgrade,
  )
where

import Control.Lens ((?=))
import Control.Lens qualified as Lens
import Control.Monad.Reader (ask)
import Data.Bifoldable (bifoldMap)
import Data.Char qualified as Char
import Data.List qualified as List
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Text.Builder qualified
import U.Codebase.Sqlite.DbId (ProjectId)
import U.Codebase.Sqlite.Project (Project (..))
import U.Util.Text qualified as Text (unsafeToInt)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as Cli
import Unison.Cli.UpdateUtils (getNamespaceDependentsOf, hydrateRefs, makeUniqueTypeGuids, nameHydratedRefIds, parseAndTypecheck, subtractDependents)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Editor.HandleInput.Branch (CreateFrom (..))
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.HandleInput.Update2 (typecheckedUnisonFileToBranchUpdates)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.SqliteCodebase.Operations qualified as Operations
import Unison.DeclCoherencyCheck qualified as DeclCoherencyCheck
import Unison.DeclNameLookup (DeclNameLookup (..))
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Project (ProjectBranchName)
import Unison.Reference (TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Sqlite (Transaction)
import Unison.Syntax.FilePrinter (renderDefnsForUnisonFile)
import Unison.Syntax.NameSegment qualified as NameSegment (toEscapedText)
import Unison.UnconflictedLocalDefnsView qualified
import Unison.Util.Alphabetical (sortAlphabeticallyOn)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, zipDefnsWith)
import Unison.Util.Map qualified as Map
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Witch (unsafeFrom)

handleUpgrade :: NameSegment -> NameSegment -> Cli ()
handleUpgrade oldName newName = do
  env <- ask
  pp <- Cli.getCurrentProjectPath

  when pp.branch.isUpdate (Cli.returnEarly (Output.CantDoThatDuring "an update" "update"))
  when pp.branch.isUpgrade (Cli.returnEarly (Output.CantDoThatDuring "an upgrade" "upgrade"))
  when pp.branch.isMerge (Cli.returnEarly (Output.CantDoThatDuring "a merge" "merge"))

  let makeUpgradeInfo (oldName, newName) = do
        when (oldName == newName) do
          Cli.returnEarlyWithoutOutput

        let oldPath = Path.Absolute (Path.fromList [NameSegment.libSegment, oldName])
        let newPath = Path.Absolute (Path.fromList [NameSegment.libSegment, newName])

        oldNamespace <- Cli.expectBranch0AtPath' (Path.AbsolutePath' oldPath)
        newLocalDefns <- Branch.deepDefns . Branch.deleteLibdeps <$> Cli.expectBranch0AtPath' (Path.AbsolutePath' newPath)

        pure
          UpgradeInfo
            { oldName,
              oldNamespace,
              oldDeepDefns = Branch.deepDefns oldNamespace,
              oldLocalDefns = Branch.deepDefns (Branch.deleteLibdeps oldNamespace),
              newName,
              newLocalDefns
            }

  currentNamespace <- Cli.getCurrentProjectRoot
  let currentNamespace0 = Branch.head currentNamespace

  upgradeInfo <- makeUpgradeInfo (oldName, newName)

  let upgradeInfos =
        [upgradeInfo]

  let currentNamespaceSansOlds0 =
        List.foldl' (\acc info -> Branch.deleteLibdep info.oldName acc) currentNamespace0 upgradeInfos
  let currentDeepDefnsSansOlds = Branch.deepDefns currentNamespaceSansOlds0

  -- Assert that the namespace doesn't have any conflicted names
  unconflictedView <-
    Branch.asUnconflicted currentNamespace0
      & onLeft (Cli.returnEarly . Output.ConflictedDefn)

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

  (declNameLookup, dependents, dependentsRefs, hydratedDependents) <-
    Cli.runTransactionWithRollback \rollback -> do
      -- Assert that the namespace doesn't have any incoherent decls
      declNameLookup <-
        Codebase.getBranchDeclNameLookup env.codebase (Branch.namespaceHash currentNamespace) unconflictedView
          & onLeftM (rollback . Output.IncoherentDeclDuringUpgrade . DeclCoherencyCheck.asOneRandomIncoherentDeclReason)

      dependents <-
        getNamespaceDependentsOf
          unconflictedView.defns
          (upgradeInfosToDependencies upgradeInfos (Branch.deepDefnsRefs currentNamespaceSansOlds0))

      let dependentsRefs :: DefnsF Set TermReferenceId TypeReferenceId
          dependentsRefs =
            bimap Map.elemsSet Map.elemsSet dependents

      hydratedDependents0 <-
        hydrateRefs
          (Codebase.unsafeGetTermComponent env.codebase)
          Operations.expectDeclComponent
          dependentsRefs

      let hydratedDependents1 =
            nameHydratedRefIds dependents hydratedDependents0

      pure (declNameLookup, dependents, dependentsRefs, hydratedDependents1)

  let prettyUnisonFile =
        makePrettyUnisonFile $
          renderDefnsForUnisonFile
            declNameLookup
            ( PPED.leftBiased
                [ makeOldDepPPE [upgradeInfo] currentDeepDefnsSansOlds,
                  PPED.makePPED
                    (PPE.namer (Names.fromUnconflictedReferenceIds dependents))
                    (PPE.suffixifyByName (Names.fromRelations currentDeepDefnsSansOlds)),
                  PPED.makePPED
                    (PPE.hqNamer 10 (Names.fromRelations currentDeepDefnsSansOlds))
                    (PPE.suffixifyByHash (Names.fromRelations currentDeepDefnsSansOlds))
                ]
            )
            Set.empty
            (over (#terms . Lens.mapped) snd hydratedDependents)

  parsingEnv <-
    Cli.makeParsingEnv pp (Names.fromRelations currentDeepDefnsSansOlds)

  typecheckedUnisonFile <- do
    parseAndTypecheck prettyUnisonFile parsingEnv & onNothingM do
      uniqueTypeGuidsByName <-
        Cli.runTransaction (makeUniqueTypeGuids (BiMultimap.range unconflictedView.defns.types))

      _ <-
        HandleInput.Branch.createBranch
          textualDescriptionOfUpgrade
          ( CreateFrom'Upgrade
              (pp.branch, Branch.headHash currentNamespace, uniqueTypeGuidsByName)
              ( unconflictedView.defns
                  & bimap BiMultimap.range BiMultimap.range
                  & subtractDependents dependentsRefs
                  & Branch.fromUnconflictedDefns
                  & Branch.setLibdeps
                    (Branch.getAt0 (Path.singleton NameSegment.libSegment) currentNamespaceSansOlds0)
                  & (`Branch.cons` currentNamespace)
              )
          )
          pp.project
          (findTemporaryBranchName pp.project.projectId oldName newName)
      scratchFilePath <-
        Cli.getLatestFile <&> \case
          Nothing -> "scratch.u"
          Just (file, _) -> file
      #latestFile ?= (scratchFilePath, True)
      liftIO $ env.writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile) True
      Cli.returnEarly (Output.UpgradeFailure pp.branch.name scratchFilePath oldName newName)

  branchUpdates <-
    Cli.runTransactionWithRollback \abort -> do
      Codebase.addDefsToCodebase env.codebase typecheckedUnisonFile
      typecheckedUnisonFileToBranchUpdates
        abort
        (\typeName -> Right (Map.lookup typeName declNameLookup.declToConstructors))
        typecheckedUnisonFile

  -- If new name ends in `__N`, that looks like a name we generated due to a name clash (e.g. by installing a `main`
  -- branch of an unreleased dependency more than once), so we remove it, if possible.
  let maybeFinalName = do
        (NameSegment -> newNameWithoutSuffix, _) <-
          unsnocUnderscoreUnderscoreNumber (NameSegment.toUnescapedText newName)
        -- If the new name is `foo__2`, then we've parsed it into (`foo`, 2). We can use the name `foo` if either:
        --
        --   1. `foo` is the old name (which we're deleting, so we can reuse the name)
        --   2. `foo` isn't already taken.
        --
        guard $
          or
            [ newNameWithoutSuffix == oldName,
              not (Lens.has (Branch.libdeps_ . Lens.ix newNameWithoutSuffix) currentNamespace0)
            ]
        Just newNameWithoutSuffix

  let finalNameBranchStep =
        case maybeFinalName of
          Nothing -> id
          Just finalName ->
            over
              Branch.libdeps_
              ( Map.deleteLookupJust newName
                  >>> \(newLibdep, libdepsWithoutNewName) -> Map.insert finalName newLibdep libdepsWithoutNewName
              )

  Cli.stepAt
    textualDescriptionOfUpgrade
    ( PP.toRoot pp,
      finalNameBranchStep . Branch.deleteLibdep oldName . Branch.batchUpdates branchUpdates
    )

  Cli.respond (Output.UpgradeSuccess oldName newName maybeFinalName)
  where
    textualDescriptionOfUpgrade :: Text
    textualDescriptionOfUpgrade =
      Text.unwords ["upgrade", NameSegment.toEscapedText oldName, NameSegment.toEscapedText newName]

makePrettyUnisonFile :: DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText) -> Pretty ColorText
makePrettyUnisonFile dependents =
  "-- The definitions below no longer typecheck after upgrading."
    <> Pretty.newline
    <> "-- Please fix the errors, then run `update`."
    <> Pretty.newline
    <> Pretty.newline
    <> ( dependents
           & inAlphabeticalOrder
           & let f = foldMap (\defn -> defn <> Pretty.newline <> Pretty.newline) in bifoldMap f f
       )
  where
    inAlphabeticalOrder :: DefnsF (Map Name) a b -> DefnsF [] a b
    inAlphabeticalOrder =
      bimap f f
      where
        f = map snd . sortAlphabeticallyOn fst . Map.toList

data UpgradeInfo = UpgradeInfo
  { oldName :: NameSegment,
    oldNamespace :: Branch0 IO,
    oldDeepDefns :: Defns (Relation Referent Name) (Relation TypeReference Name),
    oldLocalDefns :: Defns (Relation Referent Name) (Relation TypeReference Name),
    newName :: NameSegment,
    newLocalDefns :: Defns (Relation Referent Name) (Relation TypeReference Name)
  }

upgradeInfosToDependencies :: [UpgradeInfo] -> DefnsF Set TermReference TypeReference -> DefnsF Set TermReference TypeReference
upgradeInfosToDependencies infos currentNamespaceSansOlds =
  fold
    [ -- old definitions that aren't in their new counterpart
      foldMap
        ( \info ->
            zipDefnsWith
              ( let f = Set.mapMaybe Referent.toTermReference . Relation.dom
                 in \old new -> f old `Set.difference` f new
              )
              (\old new -> Relation.dom old `Set.difference` Relation.dom new)
              info.oldLocalDefns
              info.newLocalDefns
        )
        infos,
      -- transitive deps that aren't named in current namespace after subtracting all old definitions
      zipDefnsWith Set.difference Set.difference transitiveDeps currentNamespaceSansOlds
    ]
  where
    transitiveDeps :: DefnsF Set TermReference TypeReference
    transitiveDeps =
      foldMap transitiveDepsOf infos

    transitiveDepsOf :: UpgradeInfo -> DefnsF Set TermReference TypeReference
    transitiveDepsOf info =
      info.oldNamespace
        & view Branch.libdeps_
        & foldMap (Branch.deepDefnsRefs . Branch.head)

makeOldDepPPE :: [UpgradeInfo] -> Defns (Relation Referent Name) (Relation TypeReference Name) -> PrettyPrintEnvDecl
makeOldDepPPE infos currentDeepNamesSansOlds =
  let makePPE suffixifier =
        PPE.PrettyPrintEnv termToNames typeToNames
        where
          inOldAndNewNamespaces ::
            (Ord ref) =>
            (Defns (Relation Referent Name) (Relation TypeReference Name) -> Relation ref Name) ->
            ref ->
            UpgradeInfo ->
            Bool
          inOldAndNewNamespaces which ref info =
            Relation.memberDom ref (which info.oldDeepDefns)
              && Relation.memberDom ref (which info.newLocalDefns)

          hasNewLocalDefnsForOldLocalNames ::
            (Ord ref) =>
            (Defns (Relation Referent Name) (Relation TypeReference Name) -> Relation ref Name) ->
            ref ->
            UpgradeInfo ->
            Bool
          hasNewLocalDefnsForOldLocalNames which ref info =
            not (Map.null (Relation.range (which info.newLocalDefns) `Map.restrictKeys` theOldLocalNames))
            where
              theOldLocalNames = Relation.lookupDom ref (which info.oldLocalDefns)

          onlyInOldNamespace ::
            (Ord ref) =>
            (Defns (Relation Referent Name) (Relation TypeReference Name) -> Relation ref Name) ->
            ref ->
            UpgradeInfo ->
            Bool
          onlyInOldNamespace which ref info =
            inOldNamespace && not inCurrentNamespaceSansOlds
            where
              inOldNamespace :: Bool
              inOldNamespace =
                Relation.memberDom ref (which info.oldDeepDefns)

              inCurrentNamespaceSansOlds :: Bool
              inCurrentNamespaceSansOlds =
                Relation.memberDom ref (which currentDeepNamesSansOlds)

          termToNames :: Referent -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
          termToNames ref
            | any (inOldAndNewNamespaces (.terms) ref) infos = []
            | Just info <- List.find (hasNewLocalDefnsForOldLocalNames (.terms) ref) infos =
                PPE.makeTermNames (fakeLocalNames info) suffixifier ref
            | Just info <- List.find (onlyInOldNamespace (.terms) ref) infos =
                PPE.makeTermNames (fullOldDeepNames info) PPE.dontSuffixify ref
            | otherwise = []

          typeToNames :: TypeReference -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
          typeToNames ref
            | any (inOldAndNewNamespaces (.types) ref) infos = []
            | Just info <- List.find (hasNewLocalDefnsForOldLocalNames (.types) ref) infos =
                PPE.makeTypeNames (fakeLocalNames info) suffixifier ref
            | Just info <- List.find (onlyInOldNamespace (.types) ref) infos =
                PPE.makeTypeNames (fullOldDeepNames info) PPE.dontSuffixify ref
            | otherwise = []
   in PrettyPrintEnvDecl
        { unsuffixifiedPPE = makePPE PPE.dontSuffixify,
          suffixifiedPPE = makePPE (PPE.suffixifyByHash (Names.fromRelations currentDeepNamesSansOlds))
        }
  where
    -- "full" means "with lib.old.* prefix"
    fullOldDeepNames info =
      PPE.namer $
        Names.prefix0
          (Name.fromReverseSegments (info.oldName :| [NameSegment.libSegment]))
          (Names.fromRelations info.oldDeepDefns)
    fakeLocalNames info =
      PPE.namer $
        Names.prefix0
          (Name.fromReverseSegments (info.newName :| [NameSegment.libSegment]))
          (Names.fromRelations info.oldLocalDefns)

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

-- >>> unsnocUnderscoreUnderscoreNumber "unison_base_main__13"
-- Just ("unison_base_main",13)
--
-- >>> unsnocUnderscoreUnderscoreNumber "unison_base_4_0_2"
-- Nothing
unsnocUnderscoreUnderscoreNumber :: Text -> Maybe (Text, Int)
unsnocUnderscoreUnderscoreNumber text =
  let digits = Text.takeWhileEnd Char.isDigit text
      numDigits = Text.length digits
   in if numDigits > 0 && ("__" `Text.isSuffixOf` Text.dropEnd numDigits text)
        then Just (Text.dropEnd (numDigits + 2) text, Text.unsafeToInt digits)
        else Nothing
