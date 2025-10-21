module Unison.Codebase.Editor.HandleInput.DiffBranch
  ( handleDiffBranch,
  )
where

import Control.Lens (mapped)
import Control.Monad.Reader (ask)
import Data.Bifoldable (bifoldMap)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import System.Environment (lookupEnv)
import System.Process qualified as Process
import Text.Builder qualified
import Text.Builder qualified as Text (Builder)
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project qualified as Sqlite
import Unison.Cli.DirectoryUtils (makeMakeTempFilename)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Cli.UpdateUtils qualified as UpdateUtils
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.Input (DiffBranchArg (..))
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.ShortCausalHash qualified as ShortCausalHash
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import Unison.DeclCoherencyCheck (asOneRandomIncoherentDeclReason)
import Unison.DeclNameLookup (DeclNameLookup)
import Unison.Merge qualified as Merge
import Unison.Merge.ThreeWay qualified as Merge.ThreeWay
import Unison.Merge.TwoOrThreeWay qualified as TwoOrThreeWay
import Unison.Name (Name)
import Unison.NamesUtils qualified as NamesUtils
import Unison.Parser.Ann (Ann)
import Unison.PartialDeclNameLookup qualified as PartialDeclNameLookup
import Unison.Prelude
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.Project (ProjectAndBranch (..), projectBranchNameToValidProjectBranchNameText)
import Unison.Reference (TermReferenceId, TypeReferenceId)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ReferentPrime qualified as Referent
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.FilePrinter qualified as FilePrinter
import Unison.Syntax.Name qualified as Name
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.UnconflictedLocalDefnsView (UnconflictedLocalDefnsView (..))
import Unison.Util.Alphabetical (sortAlphabeticallyOn)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.ColorText (ColorText)
import Unison.Util.Defns (Defns (..), DefnsF, defnsAreEmpty, zipDefns, zipDefnsWith)
import Unison.Util.Defns qualified as Defns
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

      -- Temporary restriction: we just don't support diffing unrelated branches
      -- In the future: we think we want to set LCA=Alice in this case?

      causalHashes <-
        TwoOrThreeWay.toThreeWayA
          (abort (Output.Literal "Sorry, I can't yet compute the difference between branches that don't have any history in common."))
          causalHashes0

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

  maybeDifftoolResult <-
    liftIO (lookupEnv "UCM_DIFFTOOL") >>= \case
      Nothing -> pure Nothing
      Just difftool0 -> do
        -- Identify the set of all names changed (added, deleted, updated) on both branches.
        let changedNames :: DefnsF Set Name Name
            changedNames =
              foldMap (bimap Map.keysSet Map.keysSet) diffblob.diffsFromLCA

        -- Get the sets of references referred to by those names on all three branches.
        let defnsToHydrate :: DefnsF Set TermReferenceId TypeReferenceId
            defnsToHydrate =
              diffblob.defns
                & foldMap \defns ->
                  defns.defns
                    & NamesUtils.restrictNames changedNames
                    & NamesUtils.forgetNames
                    & NamesUtils.referentsToIds

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

        -- Whoops! Couple things.
        --
        -- 1. Builtins. We're only rendering derived things. What happened there?
        -- 2. Libdeps differences. Those don't really have a syntax in the file. Nonetheless we can do... something in a
        -- comment I guess. Merge doesn't have this problem because libdeps are just merged and in scope on merge branch.

        exitCode <-
          liftIO do
            for_
              ( (,,,)
                  <$> filenames
                  <*> ( diffblob.declNameLookups
                          & over #lca (PartialDeclNameLookup.toDeclNameLookup Name.unsafeParseText)
                          & Merge.ThreeWay.gtoThreeWay
                      )
                  <*> namespaces
                  <*> diffblob.defns
              )
              \(name, declNameLookup, namespace, defns) ->
                env.writeSource name (renderUnisonFile declNameLookup namespace defns hydratedDefns) True
            let createProcess = (Process.shell (Text.unpack difftool)) {Process.delegate_ctlc = True}
            Process.withCreateProcess createProcess \_ _ _ -> Process.waitForProcess

        pure (Just (difftool, exitCode))

  Cli.respond (Output.ShowBranchDiff args diffblob.diffsFromLCA maybeDifftoolResult)

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

renderDefinitions :: DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText) -> Pretty ColorText
renderDefinitions defns =
  (Map.toList defns.terms ++ Map.toList defns.types)
    & sortAlphabeticallyOn fst
    & foldMap (\(_, defn) -> defn <> Pretty.newline <> Pretty.newline)

renderUnisonFile ::
  (Monoid a, Var v) =>
  DeclNameLookup ->
  Branch0 m ->
  UnconflictedLocalDefnsView ->
  Defns (Map TermReferenceId (Term v a, Type v a)) (Map TypeReferenceId (Decl v a)) ->
  Text
renderUnisonFile declNameLookup namespace defns hydratedDefns =
  Text.pack . Pretty.toPlain 80 . renderDefinitions $
    FilePrinter.renderDefnsForUnisonFile
      declNameLookup
      (Branch.toPrettyPrintEnvDecl 10 namespace)
      Set.empty
      ( hydratedDefns
          & UpdateUtils.nameHydratedRefIds2 defns.defns
          & over (#terms . mapped) snd
      )
