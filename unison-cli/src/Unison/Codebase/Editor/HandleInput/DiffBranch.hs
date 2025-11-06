module Unison.Codebase.Editor.HandleInput.DiffBranch
  ( handleDiffBranch,
  )
where

import Control.Lens (mapped, preview)
import Control.Monad.Reader (ask)
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import System.Environment (lookupEnv)
import System.Process qualified as Process
import Text.Builder qualified
import Text.Builder qualified as Text (Builder)
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project qualified as Sqlite
import Unison.Builtin qualified as Builtin
import Unison.Cli.DirectoryUtils (makeMakeTempFilename)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Pretty (prettyCausalHash, prettyLibdepName)
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.UpdateUtils qualified as UpdateUtils
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.BuiltinAnnotation (builtinAnnotation)
import Unison.Codebase.Editor.Input (DiffBranchArg (..))
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.ShortCausalHash qualified as ShortCausalHash
import Unison.DataDeclaration (Decl, DeclOrBuiltin)
import Unison.DeclCoherencyCheck (asOneRandomIncoherentDeclReason)
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.Merge qualified as Merge
import Unison.Merge.DiffOp qualified as Merge.DiffOp
import Unison.Merge.ThreeWay qualified as Merge.ThreeWay
import Unison.Merge.TwoOrThreeWay qualified as Merge.TwoOrThreeWay
import Unison.Merge.TwoOrThreeWay qualified as TwoOrThreeWay
import Unison.Merge.TwoWay qualified as Merge.TwoWay
import Unison.Name (Name)
import Unison.NameSegment (NameSegment)
import Unison.NamesUtils qualified as NamesUtils
import Unison.OrBuiltin (OrBuiltin (..))
import Unison.Parser.Ann (Ann)
import Unison.PartialDeclNameLookup qualified as PartialDeclNameLookup
import Unison.Prelude
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import Unison.Project (ProjectAndBranch (..), projectBranchNameToValidProjectBranchNameText)
import Unison.Reference (TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.FilePrinter qualified as FilePrinter
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.NamePrinter qualified as NamePrinter
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnconflictedLocalDefnsView (UnconflictedLocalDefnsView (..))
import Unison.Util.Alphabetical (sortAlphabeticallyOn)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.ColorText (ColorText)
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF3, zipDefnsWith)
import Unison.Util.Pretty (Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Set qualified as Set
import Unison.Var (Var)

handleDiffBranch :: DiffBranchArg -> DiffBranchArg -> Cli ()
handleDiffBranch aliceArg bobArg = do
  let args = Merge.TwoWay {alice = aliceArg, bob = bobArg}

  env <- ask

  currentProject <- Cli.getCurrentProject

  (namespaces, diffblob) <-
    Cli.runTransactionWithRollback \abort -> do
      aliceAndBobCausalHashes <- traverse (resolveDiffBranchArg abort currentProject) args
      lcaCausalHash <- Operations.lca aliceAndBobCausalHashes.alice aliceAndBobCausalHashes.bob
      let causalHashes0 =
            Merge.TwoOrThreeWay
              { alice = aliceAndBobCausalHashes.alice,
                bob = aliceAndBobCausalHashes.bob,
                lca = lcaCausalHash
              }

      let causalHashes =
            TwoOrThreeWay.toThreeWay causalHashes0.alice causalHashes0

      namespaces <-
        for causalHashes (Codebase.expectBranchForHashTx env.codebase)

      let namespaces0 =
            Branch.head <$> namespaces

      defns <-
        for namespaces0 \namespace ->
          Branch.asUnconflicted namespace
            & onLeft (abort . Output.ConflictedDefn)

      declNameLookups <- do
        aliceAndBob <-
          sequence $
            ( \x y z ->
                Codebase.getBranchDeclNameLookup env.codebase (Branch.namespaceHash x) y
                  & onLeftM
                    ( abort
                        . Output.IncoherentDeclDuringDiffBranch z
                        . asOneRandomIncoherentDeclReason
                    )
            )
              <$> Merge.ThreeWay.forgetLca namespaces
              <*> Merge.ThreeWay.forgetLca defns
              <*> args
        lca <- Codebase.getBranchPartialDeclNameLookup env.codebase (Branch.namespaceHash namespaces.lca) defns.lca
        pure (Merge.ThreeWay.gfromTwoWay lca aliceAndBob)

      diffblob <-
        Merge.makeDiffblob
          Merge.emptyDiffblobLog
          (UpdateUtils.hydrateRefs env.codebase . fold)
          (\_ -> pure (Branch.toNames <$> namespaces0))
          defns
          (view Branch.libdeps_ <$> namespaces0)
          declNameLookups

      pure (namespaces0, diffblob)

  let namespacesNu :: Merge.TwoOrThreeWay (Branch0 Sqlite.Transaction)
      namespacesNu = Merge.ThreeWay.toTwoOrThreeWay namespaces

  -- Identify the set of all names changed (added, deleted, updated) on both branches.
  let changedNames :: DefnsF Set Name Name
      changedNames =
        foldMap (bimap Map.keysSet Map.keysSet) diffblob.diffsFromLCA

  -- Restrict all definitions to just those changed names (regardless of which branch changed it)
  let changedDefns :: Merge.TwoOrThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
      changedDefns =
        ( if isJust namespacesNu.lca
            then
              diffblob.defns
                & Merge.ThreeWay.toTwoOrThreeWay
            else
              diffblob.defns
                & Merge.ThreeWay.forgetLca
                & Merge.TwoWay.toTwoOrThreeWay Nothing
        )
          <&> \defns ->
            NamesUtils.restrictNames changedNames defns.defns

  -- Extract out just the builtins, to be rendered specially in the file later
  let changedBuiltinDefns :: Merge.TwoOrThreeWay (DefnsF (Map Name) Text Text)
      changedBuiltinDefns =
        changedDefns
          <&> bimap
            (Map.mapMaybe Referent.asBuiltin . BiMultimap.range)
            (Map.mapMaybe (preview Reference.t_) . BiMultimap.range)

  -- Get the sets of derived reference (to hydrate) referred to by those names on all three branches.
  let defnsToHydrate :: DefnsF Set TermReferenceId TypeReferenceId
      defnsToHydrate =
        foldMap (NamesUtils.referentsToIds . NamesUtils.forgetNames) changedDefns

  -- Identify the subsets of those references that we haven't already hydrated, during the process of producing the
  -- diffblob. This may always be the empty set in the current implementation, but doesn't hurt to check.
  let unhydratedDefns :: DefnsF Set TermReferenceId TypeReferenceId
      unhydratedDefns =
        zipDefnsWith
          Set.differenceMap
          Set.differenceMap
          defnsToHydrate
          diffblob.hydratedNarrowedDefns

  -- Hydrate those unhydrated defns
  newlyHydratedDefns <-
    Cli.runTransaction do
      UpdateUtils.hydrateRefs env.codebase unhydratedDefns

  -- Make the full set of hydrated defns
  let hydratedDefns ::
        Defns
          (Map TermReferenceId (Term Symbol Ann, Type Symbol Ann))
          (Map TypeReferenceId (Decl Symbol Ann))
      hydratedDefns =
        newlyHydratedDefns <> diffblob.hydratedNarrowedDefns

  maybeDifftoolResult <-
    liftIO (lookupEnv "UCM_DIFFTOOL") >>= \case
      Nothing -> pure Nothing
      Just difftool0 -> do
        -- Make a "libdeps diffs" blob suitable for rendering in files, which merely maps libdep name to its causal
        -- hash. `Nothing` means the libdep was deleted.
        let libdepsDiffs :: Merge.ThreeWay (Map NameSegment (Maybe CausalHash))
            libdepsDiffs =
              diffblob.libdepsDiffs
                & fmap
                  ( Map.merge
                      -- If this libdep only exists in the lca, but not alice/bob, that means alice/bob just didn't
                      -- touch it. But the other party did – that's how it exists in the lca blob! So, we still want it
                      -- in both renderings.
                      (Map.mapMissing \_ -> Just)
                      -- If this libdep only exists in alice/bob, not lca, it's clearly an add
                      ( Map.mapMissing \_ -> \case
                          Merge.DiffOp'Add libdep -> Just (Branch.headHash libdep)
                          -- these are impossible
                          Merge.DiffOp'Update _ -> error "expected Add"
                          Merge.DiffOp'Delete _ -> error "expected Add"
                      )
                      -- If this libdep exists in both lca and alice/bob, it's clearly not an add
                      ( Map.zipWithMatched \_ _ -> \case
                          Merge.DiffOp'Update libdeps -> Just (Branch.headHash libdeps.new)
                          Merge.DiffOp'Delete _ -> Nothing
                          -- impossible
                          Merge.DiffOp'Add _ -> error "expected Update or Delete"
                      )
                      lcaLibdepsDiff
                  )
                & Merge.TwoWay.toThreeWay (Map.map Just lcaLibdepsDiff)
              where
                -- The LCA libdeps diff is the causal hashes of every libdep updated or deleted by one party
                lcaLibdepsDiff :: Map NameSegment CausalHash
                lcaLibdepsDiff =
                  namespacesNu.lca
                    & fromMaybe namespacesNu.alice -- a missing LCA means we're treating Alice as LCA
                    & view Branch.libdeps_
                    & (`Map.restrictKeys` deletedAndUpdatedLibdepsNames)
                    & Map.map Branch.headHash

                -- Identify the names of the libdeps that were deleted or updated on alice & bob.
                deletedAndUpdatedLibdepsNames :: Set NameSegment
                deletedAndUpdatedLibdepsNames =
                  foldMap
                    ( Map.foldMapWithKey \name -> \case
                        Merge.DiffOp'Add _ -> Set.empty
                        Merge.DiffOp'Update _ -> Set.singleton name
                        Merge.DiffOp'Delete _ -> Set.singleton name
                    )
                    diffblob.libdepsDiffs

        makeTempFilename <-
          makeMakeTempFilename

        let filenames =
              fmap
                makeTempFilename
                Merge.ThreeWay
                  { lca = slugs.alice <> "-" <> slugs.bob <> "-merged.u",
                    alice = slugs.alice <> ".u",
                    bob = slugs.bob <> ".u"
                  }
              where
                slugs =
                  mangleDiffBranchArg <$> args

        let difftool =
              difftool0
                & Text.pack
                & Text.replace "$BASE" filenames.lca
                & Text.replace "$LOCAL" filenames.alice
                & Text.replace "$MERGED" filenames.lca
                & Text.replace "$REMOTE" filenames.bob

        exitCode <-
          liftIO do
            let renderedUnisonFiles :: Merge.ThreeWay Text
                renderedUnisonFiles =
                  Merge.TwoWay.toThreeWay
                    ( -- These either both have a Nothing lca or both have a Just lca
                      case (namespacesNu.lca, changedBuiltinDefns.lca) of
                        (Just lca, Just builtins) ->
                          renderUnisonFile
                            -- FIXME whoops, we can't always `unsafeParseText` out of a missing name in the LCA here...
                            -- need a rendering function that knows how to print decls with missing names, I guess
                            (PartialDeclNameLookup.toDeclNameLookup Name.unsafeParseText diffblob.declNameLookups.lca)
                            lca
                            libdepsDiffs.lca
                            diffblob.defns.lca
                            builtins
                            hydratedDefns
                        _ -> aliceAndBobFiles.alice
                    )
                    aliceAndBobFiles
                  where
                    aliceAndBobFiles :: Merge.TwoWay Text
                    aliceAndBobFiles =
                      renderUnisonFile
                        <$> Merge.ThreeWay.gforgetLca diffblob.declNameLookups
                        <*> Merge.TwoOrThreeWay.forgetLca namespacesNu
                        <*> Merge.ThreeWay.forgetLca libdepsDiffs
                        <*> Merge.ThreeWay.forgetLca diffblob.defns
                        <*> Merge.TwoOrThreeWay.forgetLca changedBuiltinDefns
                        <*> pure hydratedDefns

            for_ ((,) <$> filenames <*> renderedUnisonFiles) \(name, contents) ->
              env.writeSource name contents True
            let createProcess = (Process.shell (Text.unpack difftool)) {Process.delegate_ctlc = True}
            Process.withCreateProcess createProcess \_ _ _ -> Process.waitForProcess

        pure (Just (difftool, exitCode))

  let typeRefToDeclOrBuiltin :: TypeReference -> DeclOrBuiltin Symbol Ann
      typeRefToDeclOrBuiltin = \case
        Reference.DerivedId refId -> NotBuiltin (hydratedDefns.types Map.! refId)
        Reference.Builtin builtin -> Builtin (Builtin.expectBuiltinConstructorType builtin)

  let termRefToType :: TermReference -> Type Symbol Ann
      termRefToType = \case
        Reference.DerivedId refId -> snd (hydratedDefns.terms Map.! refId)
        Reference.Builtin builtin -> const builtinAnnotation <$> Builtin.expectBuiltinTermType builtin

  let newTypes ::
        DefnsF3 (Map Name) Merge.DiffOp Merge.Synhashed Referent TypeReference ->
        Map Name (DeclOrBuiltin Symbol Ann)
      newTypes defns =
        defns.types & Map.mapMaybe \case
          Merge.DiffOp'Add ref -> Just (typeRefToDeclOrBuiltin ref.value)
          _ -> Nothing

  let updatedTypes ::
        DefnsF3 (Map Name) Merge.DiffOp Merge.Synhashed Referent TypeReference ->
        Map Name (DeclOrBuiltin Symbol Ann)
      updatedTypes defns =
        defns.types & Map.mapMaybe \case
          Merge.DiffOp'Update refs -> Just (typeRefToDeclOrBuiltin refs.new.value)
          _ -> Nothing

  let deletedTypes ::
        DefnsF3 (Map Name) Merge.DiffOp Merge.Synhashed Referent TypeReference ->
        Map Name (DeclOrBuiltin Symbol Ann)
      deletedTypes defns =
        defns.types & Map.mapMaybe \case
          Merge.DiffOp'Delete ref -> Just (typeRefToDeclOrBuiltin ref.value)
          _ -> Nothing

  let newTerms ::
        DefnsF3 (Map Name) Merge.DiffOp Merge.Synhashed Referent TypeReference ->
        Map Name (Type Symbol Ann)
      newTerms defns =
        defns.terms & Map.mapMaybe \case
          Merge.DiffOp'Add ref | Referent.Ref ref1 <- ref.value -> Just (termRefToType ref1)
          _ -> Nothing

  let updatedTerms ::
        DefnsF3 (Map Name) Merge.DiffOp Merge.Synhashed Referent TypeReference ->
        Map Name (Type Symbol Ann)
      updatedTerms defns =
        defns.terms & Map.mapMaybe \case
          Merge.DiffOp'Update refs | Referent.Ref ref1 <- refs.new.value -> Just (termRefToType ref1)
          _ -> Nothing

  let deletedTerms ::
        DefnsF3 (Map Name) Merge.DiffOp Merge.Synhashed Referent TypeReference ->
        Map Name (Type Symbol Ann)
      deletedTerms defns =
        defns.terms & Map.mapMaybe \case
          Merge.DiffOp'Delete ref | Referent.Ref ref1 <- ref.value -> Just (termRefToType ref1)
          _ -> Nothing

  let diffs ::
        Merge.TwoWay
          ( Defns
              ( Map Name (Type Symbol Ann),
                Map Name (Type Symbol Ann),
                Map Name (Type Symbol Ann)
              )
              ( Map Name (DeclOrBuiltin Symbol Ann),
                Map Name (DeclOrBuiltin Symbol Ann),
                Map Name (DeclOrBuiltin Symbol Ann)
              )
          )
      diffs =
        diffblob.diffsFromLCA <&> \diff ->
          Defns
            { terms = (newTerms diff, updatedTerms diff, deletedTerms diff),
              types = (newTypes diff, updatedTypes diff, deletedTypes diff)
            }

  Cli.respond $
    Output.ShowBranchDiff
      args
      ((.suffixifiedPPE) . Branch.toPrettyPrintEnvDecl 10 <$> Merge.TwoOrThreeWay.forgetLca namespacesNu)
      (Map.map (Merge.DiffOp.map Branch.headHash) <$> diffblob.libdepsDiffs)
      diffs
      maybeDifftoolResult

resolveDiffBranchArg ::
  (forall void. Output -> Sqlite.Transaction void) ->
  Sqlite.Project ->
  DiffBranchArg ->
  Sqlite.Transaction CausalHash
resolveDiffBranchArg abort currentProject = \case
  DiffBranchArg'Branch names -> do
    projectAndBranch <-
      ProjectUtils.expectProjectAndBranchByTheseNamesTx abort currentProject case names.project of
        Nothing -> That names.branch
        Just projectName -> These projectName names.branch
    ProjectUtils.getProjectBranchCausalHash projectAndBranch.branch
  DiffBranchArg'Hash hash -> Cli.resolveShortCausalHashToCausalHash abort hash

-- | Mangle a diff branch arg into a text. It's only used to make a somewhat recognizable temp file name.
mangleDiffBranchArg :: DiffBranchArg -> Text.Builder
mangleDiffBranchArg = \case
  DiffBranchArg'Branch branch -> projectBranchNameToValidProjectBranchNameText branch.branch
  DiffBranchArg'Hash hash -> Text.Builder.text (ShortCausalHash.toText hash)

renderUnisonFile ::
  (Monoid a, Var v) =>
  DeclNameLookup ->
  Branch0 m ->
  Map NameSegment (Maybe CausalHash) ->
  UnconflictedLocalDefnsView ->
  DefnsF (Map Name) Text Text ->
  Defns (Map TermReferenceId (Term v a, Type v a)) (Map TypeReferenceId (Decl v a)) ->
  Text
renderUnisonFile declNameLookup namespace libdeps defns builtinDefns hydratedDefns =
  let renderedLibdeps :: Pretty ColorText
      renderedLibdeps =
        libdeps
          & Map.toList
          & sortAlphabeticallyOn fst
          & map
            ( \case
                (libdep, Nothing) -> "-- lib." <> prettyLibdepName libdep <> " ="
                (libdep, Just hash) -> "-- lib." <> prettyLibdepName libdep <> " = " <> prettyCausalHash hash
            )
          & Pretty.lines

      builtinDefns1 :: DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)
      builtinDefns1 =
        let f =
              Map.mapWithKey
                ( \name builtin ->
                    "-- "
                      <> NamePrinter.prettyName name
                      <> " refers to builtin ##"
                      <> Pretty.text builtin
                )
         in bimap f f builtinDefns

      nonBuiltinDefns :: DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)
      nonBuiltinDefns =
        FilePrinter.renderDefnsForUnisonFile
          declNameLookup
          (Branch.toPrettyPrintEnvDecl 10 namespace)
          Set.empty
          ( hydratedDefns
              & UpdateUtils.nameHydratedRefIds2 defns.defns
              & over (#terms . mapped) snd
          )

      renderedDefns :: Pretty ColorText
      renderedDefns =
        zipDefnsWith Map.union Map.union builtinDefns1 nonBuiltinDefns
          & (\defns -> Map.toList defns.terms ++ Map.toList defns.types)
          & sortAlphabeticallyOn fst
          & foldMap (\(_, defn) -> defn <> Pretty.newline <> Pretty.newline)
   in Pretty.toPlain 80 (Pretty.sepNonEmpty "\n\n" [renderedLibdeps, renderedDefns])
