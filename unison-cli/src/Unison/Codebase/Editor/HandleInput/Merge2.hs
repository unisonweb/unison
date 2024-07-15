-- | @merge@ input handler.
module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,

    -- * API exported for @pull@
    MergeInfo (..),
    AliceMergeInfo (..),
    BobMergeInfo (..),
    LcaMergeInfo (..),
    doMerge,
    doMergeLocalBranch,

    -- * API exported for @todo@
    hasDefnsInLib,
  )
where

import Control.Lens (mapped, _1)
import Control.Monad.Reader (ask)
import Control.Monad.Writer (Writer)
import Control.Monad.Writer qualified as Writer
import Data.Bifoldable (bifoldMap)
import Data.Bitraversable (bitraverse)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty (pattern (:|))
import Data.List.NonEmpty qualified as List.NonEmpty
import Data.Map.Strict qualified as Map
import Data.Semialign (align, unzip)
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as Set.NonEmpty
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These (These (..))
import Text.ANSI qualified as Text
import Text.Builder qualified
import Text.Builder qualified as Text (Builder)
import U.Codebase.Branch qualified as V2 (Branch (..), CausalBranch)
import U.Codebase.Branch qualified as V2.Branch
import U.Codebase.Causal qualified as V2.Causal
import U.Codebase.HashTags (CausalHash, unCausalHash)
import U.Codebase.Reference (Reference, TermReferenceId, TypeReference, TypeReferenceId)
import U.Codebase.Referent qualified as V2 (Referent)
import U.Codebase.Sqlite.DbId (ProjectId)
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Builtin.Decls qualified as Builtin.Decls
import Unison.Cli.MergeTypes (MergeSource (..), MergeSourceAndTarget (..), MergeSourceOrTarget (..))
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as ProjectUtils
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.HandleInput.Update2
  ( getNamespaceDependentsOf2,
    makeParsingEnv,
    prettyParseTypecheck2,
    typecheckedUnisonFileToBranchAdds,
  )
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Editor.RemoteRepo (ReadShareLooseCode (..))
import Unison.Codebase.Path (Path)
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.ProjectPath (ProjectPathG (..))
import Unison.Codebase.ProjectPath qualified as PP
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Conversions
import Unison.Codebase.SqliteCodebase.Operations qualified as Operations
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import Unison.Debug qualified as Debug
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.HashQualified qualified as HQ
import Unison.HashQualifiedPrime qualified as HQ'
import Unison.Merge.CombineDiffs (CombinedDiffOp (..), combineDiffs)
import Unison.Merge.Database (MergeDatabase (..), makeMergeDatabase, referent2to1)
import Unison.Merge.DeclCoherencyCheck (IncoherentDeclReason (..), checkDeclCoherency, lenientCheckDeclCoherency)
import Unison.Merge.DeclNameLookup (DeclNameLookup (..), expectConstructorNames)
import Unison.Merge.Diff qualified as Merge
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.EitherWayI (EitherWayI (..))
import Unison.Merge.EitherWayI qualified as EitherWayI
import Unison.Merge.Libdeps qualified as Merge
import Unison.Merge.PartialDeclNameLookup (PartialDeclNameLookup)
import Unison.Merge.PartitionCombinedDiffs (partitionCombinedDiffs)
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.Synhashed qualified as Synhashed
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.ThreeWay qualified as ThreeWay
import Unison.Merge.TwoOrThreeWay (TwoOrThreeWay (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Merge.TwoWay qualified as TwoWay
import Unison.Merge.TwoWayI qualified as TwoWayI
import Unison.Merge.Unconflicts (Unconflicts (..))
import Unison.Merge.Unconflicts qualified as Unconflicts
import Unison.Merge.Updated (Updated (..))
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment qualified as NameSegment
import Unison.NameSegment.Internal (NameSegment (NameSegment))
import Unison.NameSegment.Internal qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Prelude
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl (PrettyPrintEnvDecl (..))
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Project (ProjectAndBranch (..), ProjectBranchName, ProjectBranchNameKind (..), ProjectName, Semver (..), classifyProjectBranchName)
import Unison.Reference qualified as Reference
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.ReferentPrime qualified as Referent'
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Syntax.DeclPrinter (AccessorName)
import Unison.Syntax.DeclPrinter qualified as DeclPrinter
import Unison.Syntax.Name qualified as Name
import Unison.Syntax.TermPrinter qualified as TermPrinter
import Unison.Term (Term)
import Unison.Type (Type)
import Unison.Typechecker qualified as Typechecker
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), DefnsF, DefnsF2, DefnsF3, alignDefnsWith, defnsAreEmpty, zipDefnsWith, zipDefnsWith3)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Nametree (Nametree (..), flattenNametree, traverseNametreeWithName, unflattenNametree)
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Star2 (Star2)
import Unison.Util.Star2 qualified as Star2
import Unison.Util.SyntaxText (SyntaxText')
import Unison.Var (Var)
import Witch (unsafeFrom)
import Prelude hiding (unzip, zip, zipWith)

handleMerge :: ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleMerge (ProjectAndBranch maybeBobProjectName bobBranchName) = do
  -- Assert that Alice (us) is on a project branch, and grab the causal hash.
  ProjectPath aliceProject aliceProjectBranch _path <- Cli.getCurrentProjectPath
  let aliceProjectAndBranch = ProjectAndBranch aliceProject aliceProjectBranch

  -- Resolve Bob's maybe-project-name + branch-name to the info the merge algorithm needs: the project name, branch
  -- name, and causal hash.
  bobProject <-
    case maybeBobProjectName of
      Nothing -> pure aliceProjectAndBranch.project
      Just bobProjectName
        | bobProjectName == aliceProjectAndBranch.project.name -> pure aliceProjectAndBranch.project
        | otherwise -> do
            Cli.runTransaction (Queries.loadProjectByName bobProjectName)
              & onNothingM (Cli.returnEarly (Output.LocalProjectDoesntExist bobProjectName))
  bobProjectBranch <- ProjectUtils.expectProjectBranchByName bobProject bobBranchName
  let bobProjectAndBranch = ProjectAndBranch bobProject bobProjectBranch

  doMergeLocalBranch
    TwoWay
      { alice = aliceProjectAndBranch,
        bob = bobProjectAndBranch
      }

data MergeInfo = MergeInfo
  { alice :: !AliceMergeInfo,
    bob :: !BobMergeInfo,
    lca :: !LcaMergeInfo,
    -- | How should we describe this merge in the reflog?
    description :: !Text
  }

data AliceMergeInfo = AliceMergeInfo
  { causalHash :: !CausalHash,
    projectAndBranch :: !(ProjectAndBranch Project ProjectBranch)
  }

data BobMergeInfo = BobMergeInfo
  { causalHash :: !CausalHash,
    source :: !MergeSource
  }

newtype LcaMergeInfo = LcaMergeInfo
  { causalHash :: Maybe CausalHash
  }

doMerge :: MergeInfo -> Cli ()
doMerge info = do
  let debugFunctions =
        if Debug.shouldDebug Debug.Merge
          then realDebugFunctions
          else fakeDebugFunctions

  let aliceBranchNames = ProjectUtils.justTheNames info.alice.projectAndBranch
  let mergeSource = MergeSourceOrTarget'Source info.bob.source
  let mergeTarget = MergeSourceOrTarget'Target aliceBranchNames
  let mergeSourceAndTarget = MergeSourceAndTarget {alice = aliceBranchNames, bob = info.bob.source}

  Cli.Env {codebase} <- ask

  finalOutput <-
    Cli.label \done -> do
      -- If alice == bob, or LCA == bob (so alice is ahead of bob), then we are done.
      when (info.alice.causalHash == info.bob.causalHash || info.lca.causalHash == Just info.bob.causalHash) do
        done (Output.MergeAlreadyUpToDate2 mergeSourceAndTarget)

      -- Otherwise, if LCA == alice (so alice is behind bob), then we could fast forward to bob, so we're done.
      when (info.lca.causalHash == Just info.alice.causalHash) do
        bobBranch <- liftIO (Codebase.expectBranchForHash codebase info.bob.causalHash)
        _ <- Cli.updateAt info.description (PP.projectBranchRoot info.alice.projectAndBranch) (\_aliceBranch -> bobBranch)
        done (Output.MergeSuccessFastForward mergeSourceAndTarget)

      -- Create a bunch of cached database lookup functions
      db <- makeMergeDatabase codebase

      -- Load Alice/Bob/LCA causals
      causals <- Cli.runTransaction do
        traverse
          Operations.expectCausalBranchByCausalHash
          TwoOrThreeWay
            { alice = info.alice.causalHash,
              bob = info.bob.causalHash,
              lca = info.lca.causalHash
            }

      liftIO (debugFunctions.debugCausals causals)

      -- Load Alice/Bob/LCA branches
      branches <-
        Cli.runTransaction do
          alice <- causals.alice.value
          bob <- causals.bob.value
          lca <- for causals.lca \causal -> causal.value
          pure TwoOrThreeWay {lca, alice, bob}

      -- Assert that neither Alice nor Bob have defns in lib
      for_ [(mergeTarget, branches.alice), (mergeSource, branches.bob)] \(who, branch) -> do
        whenM (Cli.runTransaction (hasDefnsInLib branch)) do
          done (Output.MergeDefnsInLib who)

      -- Load Alice/Bob/LCA definitions and decl name lookups
      (defns3, declNameLookups, lcaDeclNameLookup) <- do
        let emptyNametree = Nametree {value = Defns Map.empty Map.empty, children = Map.empty}
        let loadDefns branch =
              Cli.runTransaction (loadNamespaceDefinitions (referent2to1 db) branch) & onLeftM \conflictedName ->
                done case conflictedName of
                  ConflictedName'Term name refs -> Output.MergeConflictedTermName name refs
                  ConflictedName'Type name refs -> Output.MergeConflictedTypeName name refs
        let load = \case
              Nothing -> pure (emptyNametree, DeclNameLookup Map.empty Map.empty)
              Just (who, branch) -> do
                defns <- loadDefns branch
                declNameLookup <-
                  Cli.runTransaction (checkDeclCoherency db.loadDeclNumConstructors defns) & onLeftM \err ->
                    done case err of
                      IncoherentDeclReason'ConstructorAlias typeName conName1 conName2 ->
                        Output.MergeConstructorAlias who typeName conName1 conName2
                      IncoherentDeclReason'MissingConstructorName name -> Output.MergeMissingConstructorName who name
                      IncoherentDeclReason'NestedDeclAlias shorterName longerName ->
                        Output.MergeNestedDeclAlias who shorterName longerName
                      IncoherentDeclReason'StrayConstructor name -> Output.MergeStrayConstructor who name
                pure (defns, declNameLookup)

        (aliceDefns0, aliceDeclNameLookup) <- load (Just (mergeTarget, branches.alice))
        (bobDefns0, bobDeclNameLookup) <- load (Just (mergeSource, branches.bob))
        lcaDefns0 <- maybe (pure emptyNametree) loadDefns branches.lca
        lcaDeclNameLookup <- Cli.runTransaction (lenientCheckDeclCoherency db.loadDeclNumConstructors lcaDefns0)

        let flatten defns = Defns (flattenNametree (view #terms) defns) (flattenNametree (view #types) defns)
        let defns3 = flatten <$> ThreeWay {alice = aliceDefns0, bob = bobDefns0, lca = lcaDefns0}
        let declNameLookups = TwoWay {alice = aliceDeclNameLookup, bob = bobDeclNameLookup}

        pure (defns3, declNameLookups, lcaDeclNameLookup)

      let defns = ThreeWay.forgetLca defns3

      liftIO (debugFunctions.debugDefns defns3 declNameLookups lcaDeclNameLookup)

      -- Diff LCA->Alice and LCA->Bob
      diffs <- Cli.runTransaction (Merge.nameBasedNamespaceDiff db declNameLookups lcaDeclNameLookup defns3)

      liftIO (debugFunctions.debugDiffs diffs)

      -- Bail early if it looks like we can't proceed with the merge, because Alice or Bob has one or more conflicted alias
      for_ ((,) <$> TwoWay mergeTarget mergeSource <*> diffs) \(who, diff) ->
        whenJust (findConflictedAlias defns3.lca diff) \(name1, name2) ->
          done (Output.MergeConflictedAliases who name1 name2)

      -- Combine the LCA->Alice and LCA->Bob diffs together
      let diff = combineDiffs diffs

      liftIO (debugFunctions.debugCombinedDiff diff)

      -- Partition the combined diff into the conflicted things and the unconflicted things
      (conflicts, unconflicts) <-
        partitionCombinedDiffs defns declNameLookups diff & onLeft \name ->
          done (Output.MergeConflictInvolvingBuiltin name)

      liftIO (debugFunctions.debugPartitionedDiff conflicts unconflicts)

      -- Identify the unconflicted dependents we need to pull into the Unison file (either first for typechecking, if there
      -- aren't conflicts, or else for manual conflict resolution without a typechecking step, if there are)
      dependents <- Cli.runTransaction (identifyDependents defns conflicts unconflicts)

      liftIO (debugFunctions.debugDependents dependents)

      let stageOne :: DefnsF (Map Name) Referent TypeReference
          stageOne =
            makeStageOne
              declNameLookups
              conflicts
              unconflicts
              dependents
              (bimap BiMultimap.range BiMultimap.range defns3.lca)

      liftIO (debugFunctions.debugStageOne stageOne)

      -- Load and merge Alice's and Bob's libdeps
      mergedLibdeps <-
        Cli.runTransaction do
          libdeps <- loadLibdeps branches
          libdepsToBranch0 db (Merge.mergeLibdeps getTwoFreshNames libdeps)

      -- Make PPE for Alice that contains all of Alice's names, but suffixified against her names + Bob's names
      let mkPpes :: TwoWay Names -> Names -> TwoWay PrettyPrintEnvDecl
          mkPpes defnsNames libdepsNames =
            defnsNames <&> \names -> PPED.makePPED (PPE.namer (names <> libdepsNames)) suffixifier
            where
              suffixifier = PPE.suffixifyByName (fold defnsNames <> libdepsNames)
      let ppes = mkPpes (defnsToNames <$> defns) (Branch.toNames mergedLibdeps)

      hydratedThings <- do
        Cli.runTransaction do
          for ((,) <$> conflicts <*> dependents) \(conflicts1, dependents1) ->
            let hydrate = hydrateDefns (Codebase.unsafeGetTermComponent codebase) Operations.expectDeclComponent
             in (,) <$> hydrate conflicts1 <*> hydrate dependents1

      let (renderedConflicts, renderedDependents) =
            let honk declNameLookup ppe defns =
                  let (types, accessorNames) =
                        Writer.runWriter $
                          defns.types & Map.traverseWithKey \name (ref, typ) ->
                            renderTypeBinding
                              -- Sort of a hack; since the decl printer looks in the PPE for names of constructors,
                              -- we just delete all term names out and add back the constructors...
                              -- probably no need to wipe out the suffixified side but we do it anyway
                              (setPpedToConstructorNames declNameLookup name ref ppe)
                              name
                              ref
                              typ
                      terms =
                        defns.terms & Map.mapMaybeWithKey \name (term, typ) ->
                          if Set.member name accessorNames
                            then Nothing
                            else Just (renderTermBinding ppe.suffixifiedPPE name term typ)
                   in Defns {terms, types}
             in unzip $
                  ( \declNameLookup (conflicts, dependents) ppe ->
                      let honk1 = honk declNameLookup ppe
                       in (honk1 conflicts, honk1 dependents)
                  )
                    <$> declNameLookups
                    <*> hydratedThings
                    <*> ppes

      let prettyUnisonFile =
            makePrettyUnisonFile
              TwoWay
                { alice = into @Text aliceBranchNames,
                  bob =
                    case info.bob.source of
                      MergeSource'LocalProjectBranch bobBranchNames -> into @Text bobBranchNames
                      MergeSource'RemoteProjectBranch bobBranchNames
                        | aliceBranchNames == bobBranchNames -> "remote " <> into @Text bobBranchNames
                        | otherwise -> into @Text bobBranchNames
                      MergeSource'RemoteLooseCode info ->
                        case Path.toName info.path of
                          Nothing -> "<root>"
                          Just name -> Name.toText name
                }
              renderedConflicts
              renderedDependents

      let stageOneBranch = defnsAndLibdepsToBranch0 codebase stageOne mergedLibdeps

      maybeTypecheckedUnisonFile <-
        let thisMergeHasConflicts =
              -- Eh, they'd either both be null, or neither, but just check both maps anyway
              not (defnsAreEmpty conflicts.alice) || not (defnsAreEmpty conflicts.bob)
         in if thisMergeHasConflicts
              then pure Nothing
              else do
                currentPath <- Cli.getCurrentProjectPath
                parsingEnv <- makeParsingEnv currentPath (Branch.toNames stageOneBranch)
                prettyParseTypecheck2 prettyUnisonFile parsingEnv <&> eitherToMaybe

      let parents =
            (\causal -> (causal.causalHash, Codebase.expectBranchForHash codebase causal.causalHash)) <$> causals

      case maybeTypecheckedUnisonFile of
        Nothing -> do
          Cli.Env {writeSource} <- ask
          (_temporaryBranchId, temporaryBranchName) <-
            HandleInput.Branch.createBranch
              info.description
              (HandleInput.Branch.CreateFrom'NamespaceWithParent info.alice.projectAndBranch.branch (Branch.mergeNode stageOneBranch parents.alice parents.bob))
              info.alice.projectAndBranch.project
              (findTemporaryBranchName info.alice.projectAndBranch.project.projectId mergeSourceAndTarget)

          scratchFilePath <-
            Cli.getLatestFile <&> \case
              Nothing -> "scratch.u"
              Just (file, _) -> file
          liftIO $ writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile)
          pure (Output.MergeFailure scratchFilePath mergeSourceAndTarget temporaryBranchName)
        Just tuf -> do
          Cli.runTransaction (Codebase.addDefsToCodebase codebase tuf)
          let stageTwoBranch = Branch.batchUpdates (typecheckedUnisonFileToBranchAdds tuf) stageOneBranch
          Cli.updateProjectBranchRoot_
            info.alice.projectAndBranch.branch
            info.description
            (\_aliceBranch -> Branch.mergeNode stageTwoBranch parents.alice parents.bob)
          pure (Output.MergeSuccess mergeSourceAndTarget)

  Cli.respond finalOutput

doMergeLocalBranch :: TwoWay (ProjectAndBranch Project ProjectBranch) -> Cli ()
doMergeLocalBranch branches = do
  (aliceCausalHash, bobCausalHash, lcaCausalHash) <-
    Cli.runTransaction do
      aliceCausalHash <- ProjectUtils.getProjectBranchCausalHash (branches.alice ^. #branch)
      bobCausalHash <- ProjectUtils.getProjectBranchCausalHash (branches.bob ^. #branch)
      -- Using Alice and Bob's causal hashes, find the LCA (if it exists)
      lcaCausalHash <- Operations.lca aliceCausalHash bobCausalHash
      pure (aliceCausalHash, bobCausalHash, lcaCausalHash)

  -- Do the merge!
  doMerge
    MergeInfo
      { alice =
          AliceMergeInfo
            { causalHash = aliceCausalHash,
              projectAndBranch = branches.alice
            },
        bob =
          BobMergeInfo
            { causalHash = bobCausalHash,
              source = MergeSource'LocalProjectBranch (ProjectUtils.justTheNames branches.bob)
            },
        lca =
          LcaMergeInfo
            { causalHash = lcaCausalHash
            },
        description = "merge " <> into @Text (ProjectUtils.justTheNames branches.bob)
      }

------------------------------------------------------------------------------------------------------------------------
-- Loading basic info out of the database

loadLibdeps ::
  TwoOrThreeWay (V2.Branch Transaction) ->
  Transaction (ThreeWay (Map NameSegment (V2.CausalBranch Transaction)))
loadLibdeps branches = do
  lca <-
    case branches.lca of
      Nothing -> pure Map.empty
      Just lcaBranch -> load lcaBranch
  alice <- load branches.alice
  bob <- load branches.bob
  pure ThreeWay {lca, alice, bob}
  where
    load :: V2.Branch Transaction -> Transaction (Map NameSegment (V2.CausalBranch Transaction))
    load branch =
      case Map.lookup NameSegment.libSegment branch.children of
        Nothing -> pure Map.empty
        Just libdepsCausal -> do
          libdepsBranch <- libdepsCausal.value
          pure libdepsBranch.children

------------------------------------------------------------------------------------------------------------------------
-- Merge precondition violation checks

hasDefnsInLib :: Applicative m => V2.Branch m -> m Bool
hasDefnsInLib branch = do
  libdeps <-
    case Map.lookup NameSegment.libSegment branch.children of
      Nothing -> pure V2.Branch.empty
      Just libdeps -> libdeps.value
  pure (not (Map.null libdeps.terms) || not (Map.null libdeps.types))

------------------------------------------------------------------------------------------------------------------------
-- Creating Unison files

hydrateDefns ::
  (Monad m, Ord name) =>
  (Hash -> m [term]) ->
  (Hash -> m [typ]) ->
  DefnsF (Map name) TermReferenceId TypeReferenceId ->
  m (DefnsF (Map name) term (TypeReferenceId, typ))
hydrateDefns getTermComponent getTypeComponent = do
  bitraverse (hydrateTerms getTermComponent) (hydrateTypes getTypeComponent)

hydrateTerms :: (Monad m, Ord name) => (Hash -> m [term]) -> Map name TermReferenceId -> m (Map name term)
hydrateTerms getTermComponent terms =
  componenty getTermComponent terms \_ _ -> id

hydrateTypes ::
  (Monad m, Ord name) =>
  (Hash -> m [typ]) ->
  Map name TypeReferenceId ->
  m (Map name (TypeReferenceId, typ))
hydrateTypes getTypeComponent types =
  componenty getTypeComponent types \_ -> (,)

componenty ::
  forall a b name m.
  (Monad m, Ord name) =>
  (Hash -> m [a]) ->
  Map name Reference.Id ->
  (name -> Reference.Id -> a -> b) ->
  m (Map name b)
componenty getComponent things modify =
  Foldable.foldlM f Map.empty (foldMap (Set.singleton . Reference.idToHash) things)
  where
    f :: Map name b -> Hash -> m (Map name b)
    f acc hash =
      List.foldl' g acc . Reference.componentFor hash <$> getComponent hash

    g :: Map name b -> (Reference.Id, a) -> Map name b
    g acc (ref, thing) =
      Set.foldl' (h ref thing) acc (BiMultimap.lookupDom ref things2)

    h :: Reference.Id -> a -> Map name b -> name -> Map name b
    h ref thing acc name =
      Map.insert name (modify name ref thing) acc

    things2 :: BiMultimap Reference.Id name
    things2 =
      BiMultimap.fromRange things

renderTermBinding :: (Monoid a, Var v) => PrettyPrintEnv -> Name -> Term v a -> Type v a -> Pretty ColorText
renderTermBinding ppe (HQ.NameOnly -> name) term typ =
  Pretty.syntaxToColor rendered
  where
    rendered :: Pretty (SyntaxText' Reference)
    rendered =
      if Typechecker.isEqual (Builtin.Decls.testResultListType mempty) typ
        then "test> " <> TermPrinter.prettyBindingWithoutTypeSignature ppe name term
        else TermPrinter.prettyBinding ppe name term

renderTypeBinding ::
  (Var v) =>
  PrettyPrintEnvDecl ->
  Name ->
  TypeReferenceId ->
  Decl v a ->
  Writer (Set AccessorName) (Pretty ColorText)
renderTypeBinding ppe name ref decl =
  Pretty.syntaxToColor <$> DeclPrinter.prettyDeclW ppe (Reference.fromId ref) (HQ.NameOnly name) decl

setPpedToConstructorNames :: DeclNameLookup -> Name -> TypeReferenceId -> PrettyPrintEnvDecl -> PrettyPrintEnvDecl
setPpedToConstructorNames declNameLookup name ref =
  set (#unsuffixifiedPPE . #termNames) referentNames
    . set (#suffixifiedPPE . #termNames) referentNames
  where
    constructorNameMap :: Map ConstructorReference Name
    constructorNameMap =
      Map.fromList
        ( name
            & expectConstructorNames declNameLookup
            & List.zip [0 ..]
            & over (mapped . _1) (ConstructorReference (Reference.fromId ref))
        )

    referentNames :: Referent -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
    referentNames = \case
      Referent.Con conRef _ ->
        case Map.lookup conRef constructorNameMap of
          Nothing -> []
          Just conName -> let hqConName = HQ'.NameOnly conName in [(hqConName, hqConName)]
      Referent.Ref _ -> []

makePrettyUnisonFile ::
  TwoWay Text ->
  TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)) ->
  TwoWay (DefnsF (Map Name) (Pretty ColorText) (Pretty ColorText)) ->
  Pretty ColorText
makePrettyUnisonFile authors conflicts dependents =
  fold
    [ conflicts
        -- Merge the two maps together into one, remembering who authored what
        & TwoWay.twoWay (zipDefnsWith align align)
        -- Sort alphabetically
        & inAlphabeticalOrder
        -- Render each conflict, types then terms (even though a type can conflict with a term, in which case they
        -- would not be adjacent in the file), with an author comment above each conflicted thing
        & ( let f =
                  foldMap \case
                    This x -> alice x
                    That y -> bob y
                    These x y -> alice x <> bob y
                  where
                    alice = prettyBinding (Just (Pretty.text authors.alice))
                    bob = prettyBinding (Just (Pretty.text authors.bob))
             in bifoldMap f f
          ),
      -- Show message that delineates where conflicts end and dependents begin only when there are both conflicts and
      -- dependents
      let thereAre defns = TwoWay.or (not . defnsAreEmpty <$> defns)
       in if thereAre conflicts && thereAre dependents
            then
              fold
                [ "-- The definitions below are not conflicted, but they each depend on one or more\n",
                  "-- conflicted definitions above.\n\n"
                ]
            else mempty,
      dependents
        -- Merge dependents together into one map (they are disjoint)
        & TwoWay.twoWay (zipDefnsWith Map.union Map.union)
        -- Sort alphabetically
        & inAlphabeticalOrder
        -- Render each dependent, types then terms, without bothering to comment attribution
        & (let f = foldMap (prettyBinding Nothing) in bifoldMap f f)
    ]
  where
    prettyBinding maybeComment binding =
      fold
        [ case maybeComment of
            Nothing -> mempty
            Just comment -> "-- " <> comment <> "\n",
          binding,
          "\n",
          "\n"
        ]

    inAlphabeticalOrder :: DefnsF (Map Name) a b -> DefnsF [] a b
    inAlphabeticalOrder =
      bimap f f
      where
        f = map snd . List.sortOn (Name.toText . fst) . Map.toList

------------------------------------------------------------------------------------------------------------------------
--

-- Given just named term/type reference ids, fill out all names that occupy the term and type namespaces. This is simply
-- the given names plus all of the types' constructors.
--
-- For example, if the input is
--
--   declNameLookup = {
--     "Maybe" => ["Maybe.Nothing", "Maybe.Just"]
--   }
--   defns = {
--     terms = { "foo" => #foo }
--     types = { "Maybe" => #Maybe }
--   }
--
-- then the output is
--
--   defns = {
--     terms = { "foo", "Maybe.Nothing", "Maybe.Just" }
--     types = { "Maybe" }
--   }
refIdsToNames :: DeclNameLookup -> DefnsF (Map Name) term typ -> DefnsF Set Name Name
refIdsToNames declNameLookup =
  bifoldMap goTerms goTypes
  where
    goTerms :: Map Name term -> DefnsF Set Name Name
    goTerms terms =
      Defns {terms = Map.keysSet terms, types = Set.empty}

    goTypes :: Map Name typ -> DefnsF Set Name Name
    goTypes types =
      Defns
        { terms = foldMap (Set.fromList . expectConstructorNames declNameLookup) names,
          types = names
        }
      where
        names = Map.keysSet types

defnsAndLibdepsToBranch0 ::
  Codebase IO v a ->
  DefnsF (Map Name) Referent TypeReference ->
  Branch0 Transaction ->
  Branch0 IO
defnsAndLibdepsToBranch0 codebase defns libdeps =
  let -- Unflatten the collection of terms into tree, ditto for types
      nametrees :: DefnsF2 Nametree (Map NameSegment) Referent TypeReference
      nametrees =
        bimap go go defns

      -- Align the tree of terms and tree of types into one tree
      nametree :: Nametree (DefnsF (Map NameSegment) Referent TypeReference)
      nametree =
        nametrees & alignDefnsWith \case
          This terms -> Defns {terms, types = Map.empty}
          That types -> Defns {terms = Map.empty, types}
          These terms types -> Defns terms types

      -- Convert the tree to a branch0
      branch0 = nametreeToBranch0 nametree

      -- Add back the libdeps branch at path "lib"
      branch1 = Branch.setChildBranch NameSegment.libSegment (Branch.one libdeps) branch0

      -- Awkward: we have a Branch Transaction but we need a Branch IO (because reasons)
      branch2 = Branch.transform0 (Codebase.runTransaction codebase) branch1
   in branch2
  where
    go :: (Ord v) => Map Name v -> Nametree (Map NameSegment v)
    go =
      unflattenNametree . BiMultimap.fromRange

nametreeToBranch0 :: Nametree (DefnsF (Map NameSegment) Referent TypeReference) -> Branch0 m
nametreeToBranch0 nametree =
  Branch.branch0
    (rel2star defns.terms)
    (rel2star defns.types)
    (Branch.one . nametreeToBranch0 <$> nametree.children)
    Map.empty
  where
    defns :: Defns (Relation Referent NameSegment) (Relation TypeReference NameSegment)
    defns =
      bimap (Relation.swap . Relation.fromMap) (Relation.swap . Relation.fromMap) nametree.value

    rel2star :: Relation ref name -> Star2 ref name metadata
    rel2star rel =
      Star2.Star2 {fact = Relation.dom rel, d1 = rel, d2 = Relation.empty}

-- FIXME: let's come up with a better term for "dependencies" in the implementation of this function
identifyDependents ::
  TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) ->
  DefnsF Unconflicts Referent TypeReference ->
  Transaction (TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId))
identifyDependents defns conflicts unconflicts = do
  let -- The other person's (i.e. with "Alice" and "Bob" swapped) solo-deleted and solo-updated names
      theirSoloUpdatesAndDeletes :: TwoWay (DefnsF Set Name Name)
      theirSoloUpdatesAndDeletes =
        TwoWay.swap (unconflictedSoloDeletedNames <> unconflictedSoloUpdatedNames)
        where
          unconflictedSoloDeletedNames :: TwoWay (DefnsF Set Name Name)
          unconflictedSoloDeletedNames =
            bitraverse Unconflicts.soloDeletedNames Unconflicts.soloDeletedNames unconflicts

          unconflictedSoloUpdatedNames :: TwoWay (DefnsF Set Name Name)
          unconflictedSoloUpdatedNames =
            bitraverse Unconflicts.soloUpdatedNames Unconflicts.soloUpdatedNames unconflicts

  let dependencies :: TwoWay (Set Reference)
      dependencies =
        fold
          [ -- One source of dependencies: Alice's versions of Bob's unconflicted deletes and updates, and vice-versa.
            --
            -- This is name-based: if Bob updates the *name* "foo", then we go find the thing that Alice calls "foo" (if
            -- anything), no matter what its hash is.
            defnsReferences
              <$> ( zipDefnsWith BiMultimap.restrictRan BiMultimap.restrictRan
                      <$> theirSoloUpdatesAndDeletes
                      <*> defns
                  ),
            -- The other source of dependencies: Alice's own conflicted things, and ditto for Bob.
            --
            -- An example: suppose Alice has foo#alice and Bob has foo#bob, so foo is conflicted. Furthermore, suppose
            -- Alice has bar#bar that depends on foo#alice.
            --
            -- We want Alice's #alice to be considered a dependency, so that when we go off and find dependents of these
            -- dependencies to put in the scratch file for type checking and propagation, we find bar#bar.
            --
            -- Note that this is necessary even if bar#bar is unconflicted! We don't want bar#bar to be put directly
            -- into the namespace / parsing context for the conflicted merge, because it has an unnamed reference on
            -- foo#alice. It rather ought to be in the scratchfile alongside the conflicted foo#alice and foo#bob, so
            -- that when that conflict is resolved, it will propagate to bar.
            let f :: (Foldable t) => t Reference.Id -> Set Reference
                f =
                  List.foldl' (\acc ref -> Set.insert (Reference.DerivedId ref) acc) Set.empty . Foldable.toList
             in bifoldMap f f <$> conflicts
          ]

  dependents0 <-
    for ((,) <$> defns <*> dependencies) \(defns1, dependencies1) ->
      getNamespaceDependentsOf2 defns1 dependencies1

  -- There is some subset of Alice's dependents (and ditto for Bob of course) that we don't ultimately want/need to put
  -- into the scratch file: those for which any of the following are true:
  --
  --   1. It is Alice-conflicted (since we only want to return *unconflicted* things).
  --   2. It was deleted by Bob.
  --   3. It was updated by Bob and not updated by Alice.
  let dependents1 :: TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId)
      dependents1 =
        zipDefnsWith Map.withoutKeys Map.withoutKeys
          <$> dependents0
          <*> ((bimap Map.keysSet Map.keysSet <$> conflicts) <> theirSoloUpdatesAndDeletes)

  -- Of the remaining dependents, it's still possible that the maps are not disjoint. But whenever the same name key
  -- exists in Alice's and Bob's dependents, the value will either be equal (by Unison hash)...
  --
  --   { alice = { terms = {"foo" => #alice} } }
  --   { bob   = { terms = {"foo" => #alice} } }
  --
  -- ...or synhash-equal (i.e. the term or type received different auto-propagated updates)...
  --
  --   { alice = { terms = {"foo" => #alice} } }
  --   { bob   = { terms = {"foo" => #bob}   } }
  --
  -- So, we can arbitrarily keep Alice's, because they will render the same.
  --
  --   { alice = { terms = {"foo" => #alice} } }
  --   { bob   = { terms = {}                } }
  let dependents2 :: TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId)
      dependents2 =
        dependents1 & over #bob \bob ->
          zipDefnsWith Map.difference Map.difference bob dependents1.alice

  pure dependents2

makeStageOne ::
  TwoWay DeclNameLookup ->
  TwoWay (DefnsF (Map Name) termid typeid) ->
  DefnsF Unconflicts term typ ->
  TwoWay (DefnsF (Map Name) termid typeid) ->
  DefnsF (Map Name) term typ ->
  DefnsF (Map Name) term typ
makeStageOne declNameLookups conflicts unconflicts dependents =
  zipDefnsWith3 makeStageOneV makeStageOneV unconflicts (f conflicts <> f dependents)
  where
    f :: TwoWay (DefnsF (Map Name) term typ) -> DefnsF Set Name Name
    f defns =
      fold (refIdsToNames <$> declNameLookups <*> defns)

makeStageOneV :: Unconflicts v -> Set Name -> Map Name v -> Map Name v
makeStageOneV unconflicts namesToDelete =
  (`Map.withoutKeys` namesToDelete) . Unconflicts.apply unconflicts

defnsReferences :: Defns (BiMultimap Referent name) (BiMultimap TypeReference name) -> Set Reference
defnsReferences =
  bifoldMap (Set.map Referent.toReference . BiMultimap.dom) BiMultimap.dom

defnsToNames :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) -> Names
defnsToNames defns =
  Names.Names
    { terms = Relation.fromMap (BiMultimap.range defns.terms),
      types = Relation.fromMap (BiMultimap.range defns.types)
    }

findTemporaryBranchName :: ProjectId -> MergeSourceAndTarget -> Transaction ProjectBranchName
findTemporaryBranchName projectId mergeSourceAndTarget = do
  ProjectUtils.findTemporaryBranchName projectId preferred
  where
    preferred :: ProjectBranchName
    preferred =
      unsafeFrom @Text $
        Text.Builder.run $
          "merge-"
            <> mangleMergeSource mergeSourceAndTarget.bob
            <> "-into-"
            <> mangleBranchName mergeSourceAndTarget.alice.branch

    mangleMergeSource :: MergeSource -> Text.Builder
    mangleMergeSource = \case
      MergeSource'LocalProjectBranch (ProjectAndBranch _project branch) -> mangleBranchName branch
      MergeSource'RemoteProjectBranch (ProjectAndBranch _project branch) -> "remote-" <> mangleBranchName branch
      MergeSource'RemoteLooseCode info -> manglePath info.path
    mangleBranchName :: ProjectBranchName -> Text.Builder
    mangleBranchName name =
      case classifyProjectBranchName name of
        ProjectBranchNameKind'Contributor user name1 ->
          Text.Builder.text user
            <> Text.Builder.char '-'
            <> mangleBranchName name1
        ProjectBranchNameKind'DraftRelease semver -> "releases-drafts-" <> mangleSemver semver
        ProjectBranchNameKind'Release semver -> "releases-" <> mangleSemver semver
        ProjectBranchNameKind'NothingSpecial -> Text.Builder.text (into @Text name)

    manglePath :: Path -> Text.Builder
    manglePath =
      Monoid.intercalateMap "-" (Text.Builder.text . NameSegment.toUnescapedText) . Path.toList

    mangleSemver :: Semver -> Text.Builder
    mangleSemver (Semver x y z) =
      Text.Builder.decimal x
        <> Text.Builder.char '.'
        <> Text.Builder.decimal y
        <> Text.Builder.char '.'
        <> Text.Builder.decimal z

-- Load all "namespace definitions" of a branch, which are all terms and type declarations *except* those defined
-- in the "lib" namespace.
--
-- Fails if there is a conflicted name.
loadNamespaceDefinitions ::
  forall m.
  (Monad m) =>
  (V2.Referent -> m Referent) ->
  V2.Branch m ->
  m (Either ConflictedName (Nametree (DefnsF (Map NameSegment) Referent TypeReference)))
loadNamespaceDefinitions referent2to1 =
  fmap assertNamespaceHasNoConflictedNames . go (Map.delete NameSegment.libSegment)
  where
    go ::
      (forall x. Map NameSegment x -> Map NameSegment x) ->
      V2.Branch m ->
      m (Nametree (DefnsF2 (Map NameSegment) NESet Referent TypeReference))
    go f branch = do
      terms <- for branch.terms (fmap (Set.NonEmpty.fromList . List.NonEmpty.fromList) . traverse referent2to1 . Map.keys)
      let types = Map.map (Set.NonEmpty.unsafeFromSet . Map.keysSet) branch.types
      children <-
        for (f branch.children) \childCausal -> do
          child <- childCausal.value
          go id child
      pure Nametree {value = Defns {terms, types}, children}

data ConflictedName
  = ConflictedName'Term !Name !(NESet Referent)
  | ConflictedName'Type !Name !(NESet TypeReference)

-- | Assert that there are no unconflicted names in a namespace.
assertNamespaceHasNoConflictedNames ::
  Nametree (DefnsF2 (Map NameSegment) NESet Referent TypeReference) ->
  Either ConflictedName (Nametree (DefnsF (Map NameSegment) Referent TypeReference))
assertNamespaceHasNoConflictedNames =
  traverseNametreeWithName \names defns -> do
    terms <-
      defns.terms & Map.traverseWithKey \name ->
        assertUnconflicted (ConflictedName'Term (Name.fromReverseSegments (name :| names)))
    types <-
      defns.types & Map.traverseWithKey \name ->
        assertUnconflicted (ConflictedName'Type (Name.fromReverseSegments (name :| names)))
    pure Defns {terms, types}
  where
    assertUnconflicted :: (NESet ref -> ConflictedName) -> NESet ref -> Either ConflictedName ref
    assertUnconflicted conflicted refs
      | Set.NonEmpty.size refs == 1 = Right (Set.NonEmpty.findMin refs)
      | otherwise = Left (conflicted refs)

-- @findConflictedAlias namespace diff@, given an old namespace and a diff to a new namespace, will return the first
-- "conflicted alias" encountered (if any), where a "conflicted alias" is a pair of names that referred to the same
-- thing in the old namespace, but different things in the new one.
--
-- For example, if the old namespace was
--
--   foo = #foo
--   bar = #foo
--
-- and the new namespace is
--
--   foo = #baz
--   bar = #qux
--
-- then (foo, bar) is a conflicted alias.
--
-- This function currently doesn't return whether the conflicted alias is a decl or a term, but it certainly could.
findConflictedAlias ::
  (Ord term, Ord typ) =>
  Defns (BiMultimap term Name) (BiMultimap typ Name) ->
  DefnsF3 (Map Name) DiffOp Synhashed term typ ->
  Maybe (Name, Name)
findConflictedAlias defns diff =
  asum [go defns.terms diff.terms, go defns.types diff.types]
  where
    go :: forall ref. (Ord ref) => BiMultimap ref Name -> Map Name (DiffOp (Synhashed ref)) -> Maybe (Name, Name)
    go namespace diff =
      asum (map f (Map.toList diff))
      where
        f :: (Name, DiffOp (Synhashed ref)) -> Maybe (Name, Name)
        f (name, op) =
          case op of
            DiffOp'Add _ -> Nothing
            DiffOp'Delete _ -> Nothing
            DiffOp'Update hashed1 ->
              BiMultimap.lookupPreimage name namespace
                & Set.delete name
                & Set.toList
                & map (g hashed1.new)
                & asum
          where
            g :: Synhashed ref -> Name -> Maybe (Name, Name)
            g hashed1 alias =
              case Map.lookup alias diff of
                Just (DiffOp'Update hashed2) | hashed1 == hashed2.new -> Nothing
                -- If "foo" was updated but its alias "bar" was deleted, that's ok
                Just (DiffOp'Delete _) -> Nothing
                _ -> Just (name, alias)

-- Given a name like "base", try "base__1", then "base__2", etc, until we find a name that doesn't
-- clash with any existing dependencies.
getTwoFreshNames :: Set NameSegment -> NameSegment -> (NameSegment, NameSegment)
getTwoFreshNames names name0 =
  go2 0
  where
    -- if
    --   name0 = "base"
    --   names = {"base__5", "base__6"}
    -- then
    --   go2 4 = ("base__4", "base__7")
    go2 :: Integer -> (NameSegment, NameSegment)
    go2 !i
      | Set.member name names = go2 (i + 1)
      | otherwise = (name, go1 (i + 1))
      where
        name = mangled i

    -- if
    --   name0 = "base"
    --   names = {"base__5", "base__6"}
    -- then
    --   go1 5 = "base__7"
    go1 :: Integer -> NameSegment
    go1 !i
      | Set.member name names = go1 (i + 1)
      | otherwise = name
      where
        name = mangled i

    mangled :: Integer -> NameSegment
    mangled i =
      NameSegment (NameSegment.toUnescapedText name0 <> "__" <> tShow i)

libdepsToBranch0 :: MergeDatabase -> Map NameSegment (V2.CausalBranch Transaction) -> Transaction (Branch0 Transaction)
libdepsToBranch0 db libdeps = do
  let branch :: V2.Branch Transaction
      branch =
        V2.Branch
          { terms = Map.empty,
            types = Map.empty,
            patches = Map.empty,
            children = libdeps
          }

  -- We make a fresh branch cache to load the branch of libdeps.
  -- It would probably be better to reuse the codebase's branch cache.
  -- FIXME how slow/bad is this without that branch cache?
  branchCache <- Sqlite.unsafeIO newBranchCache
  Conversions.branch2to1 branchCache db.loadDeclType branch

------------------------------------------------------------------------------------------------------------------------
-- Debugging by printing a bunch of stuff out

data DebugFunctions = DebugFunctions
  { debugCausals :: TwoOrThreeWay (V2.CausalBranch Transaction) -> IO (),
    debugDefns ::
      ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
      TwoWay DeclNameLookup ->
      PartialDeclNameLookup ->
      IO (),
    debugDiffs :: TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference) -> IO (),
    debugCombinedDiff :: DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference -> IO (),
    debugPartitionedDiff ::
      TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) ->
      DefnsF Unconflicts Referent TypeReference ->
      IO (),
    debugDependents :: TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) -> IO (),
    debugStageOne :: DefnsF (Map Name) Referent TypeReference -> IO ()
  }

realDebugFunctions :: DebugFunctions
realDebugFunctions =
  DebugFunctions
    { debugCausals = realDebugCausals,
      debugDefns = realDebugDefns,
      debugDiffs = realDebugDiffs,
      debugCombinedDiff = realDebugCombinedDiff,
      debugPartitionedDiff = realDebugPartitionedDiff,
      debugDependents = realDebugDependents,
      debugStageOne = realDebugStageOne
    }

fakeDebugFunctions :: DebugFunctions
fakeDebugFunctions =
  DebugFunctions mempty mempty mempty mempty mempty mempty mempty

realDebugCausals :: TwoOrThreeWay (V2.CausalBranch Transaction) -> IO ()
realDebugCausals causals = do
  Text.putStrLn (Text.bold "\n=== Alice causal hash ===")
  Text.putStrLn (Hash.toBase32HexText (unCausalHash causals.alice.causalHash))
  Text.putStrLn (Text.bold "\n=== Bob causal hash ===")
  Text.putStrLn (Hash.toBase32HexText (unCausalHash causals.bob.causalHash))
  Text.putStrLn (Text.bold "\n=== LCA causal hash ===")
  Text.putStrLn case causals.lca of
    Nothing -> "Nothing"
    Just causal -> "Just " <> Hash.toBase32HexText (unCausalHash causal.causalHash)

realDebugDefns ::
  ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  TwoWay DeclNameLookup ->
  PartialDeclNameLookup ->
  IO ()
realDebugDefns defns declNameLookups _lcaDeclNameLookup = do
  Text.putStrLn (Text.bold "\n=== Alice definitions ===")
  debugDefns1 (bimap BiMultimap.range BiMultimap.range defns.alice)

  Text.putStrLn (Text.bold "\n=== Bob definitions ===")
  debugDefns1 (bimap BiMultimap.range BiMultimap.range defns.bob)

  Text.putStrLn (Text.bold "\n=== Alice constructor names ===")
  debugConstructorNames declNameLookups.alice.declToConstructors

  Text.putStrLn (Text.bold "\n=== Bob constructor names ===")
  debugConstructorNames declNameLookups.bob.declToConstructors

realDebugDiffs :: TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference) -> IO ()
realDebugDiffs diffs = do
  Text.putStrLn (Text.bold "\n=== LCA→Alice diff ===")
  renderDiff diffs.alice
  Text.putStrLn (Text.bold "\n=== LCA→Bob diff ===")
  renderDiff diffs.bob
  where
    renderDiff :: DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference -> IO ()
    renderDiff diff = do
      renderThings referentLabel diff.terms
      renderThings (const "type") diff.types

    renderThings :: (ref -> Text) -> Map Name (DiffOp (Synhashed ref)) -> IO ()
    renderThings label things =
      for_ (Map.toList things) \(name, op) ->
        let go color action x =
              color $
                action
                  <> " "
                  <> Text.italic (label (Synhashed.value x))
                  <> " "
                  <> Name.toText name
                  <> " #"
                  <> Hash.toBase32HexText (Synhashed.hash x)
         in Text.putStrLn case op of
              DiffOp'Add x -> go Text.green "+" x
              DiffOp'Delete x -> go Text.red "-" x
              DiffOp'Update x -> go Text.yellow "%" x.new

realDebugCombinedDiff :: DefnsF2 (Map Name) CombinedDiffOp Referent TypeReference -> IO ()
realDebugCombinedDiff diff = do
  Text.putStrLn (Text.bold "\n=== Combined diff ===")
  renderThings referentLabel Referent.toText diff.terms
  renderThings (const "type") Reference.toText diff.types
  where
    renderThings :: (ref -> Text) -> (ref -> Text) -> Map Name (CombinedDiffOp ref) -> IO ()
    renderThings label renderRef things =
      for_ (Map.toList things) \(name, op) ->
        Text.putStrLn case op of
          CombinedDiffOp'Add who ->
            Text.green $
              "+ "
                <> Text.italic (label (EitherWayI.value who))
                <> " "
                <> Name.toText name
                <> " "
                <> renderRef (EitherWayI.value who)
                <> " ("
                <> renderWho who
                <> ")"
          CombinedDiffOp'Delete who ->
            Text.red $
              "- "
                <> Text.italic (label (EitherWayI.value who))
                <> " "
                <> Name.toText name
                <> " "
                <> renderRef (EitherWayI.value who)
                <> " ("
                <> renderWho who
                <> ")"
          CombinedDiffOp'Update who ->
            Text.yellow $
              "% "
                <> Text.italic (label (EitherWayI.value who).new)
                <> " "
                <> Name.toText name
                <> " "
                <> renderRef (EitherWayI.value who).new
                <> " ("
                <> renderWho who
                <> ")"
          CombinedDiffOp'Conflict ref ->
            Text.magenta $
              "! "
                <> Text.italic (label ref.alice)
                <> "/"
                <> Text.italic (label ref.bob)
                <> " "
                <> Name.toText name
                <> " "
                <> renderRef ref.alice
                <> "/"
                <> renderRef ref.bob

    renderWho :: EitherWayI v -> Text
    renderWho = \case
      OnlyAlice _ -> "Alice"
      OnlyBob _ -> "Bob"
      AliceAndBob _ -> "Alice and Bob"

realDebugPartitionedDiff ::
  TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) ->
  DefnsF Unconflicts Referent TypeReference ->
  IO ()
realDebugPartitionedDiff conflicts unconflicts = do
  Text.putStrLn (Text.bold "\n=== Alice conflicts ===")
  renderConflicts "termid" conflicts.alice.terms (Alice ())
  renderConflicts "typeid" conflicts.alice.types (Alice ())

  Text.putStrLn (Text.bold "\n=== Bob conflicts ===")
  renderConflicts "termid" conflicts.bob.terms (Bob ())
  renderConflicts "typeid" conflicts.bob.types (Bob ())

  Text.putStrLn (Text.bold "\n=== Alice unconflicts ===")
  renderUnconflicts Text.green "+" referentLabel Referent.toText unconflicts.terms.adds.alice
  renderUnconflicts Text.green "+" (const "type") Reference.toText unconflicts.types.adds.alice
  renderUnconflicts Text.red "-" referentLabel Referent.toText unconflicts.terms.deletes.alice
  renderUnconflicts Text.red "-" (const "type") Reference.toText unconflicts.types.deletes.alice
  renderUnconflicts Text.yellow "%" referentLabel Referent.toText unconflicts.terms.updates.alice
  renderUnconflicts Text.yellow "%" (const "type") Reference.toText unconflicts.types.updates.alice

  Text.putStrLn (Text.bold "\n=== Bob unconflicts ===")
  renderUnconflicts Text.green "+" referentLabel Referent.toText unconflicts.terms.adds.bob
  renderUnconflicts Text.green "+" (const "type") Reference.toText unconflicts.types.adds.bob
  renderUnconflicts Text.red "-" referentLabel Referent.toText unconflicts.terms.deletes.bob
  renderUnconflicts Text.red "-" (const "type") Reference.toText unconflicts.types.deletes.bob
  renderUnconflicts Text.yellow "%" referentLabel Referent.toText unconflicts.terms.updates.bob
  renderUnconflicts Text.yellow "%" (const "type") Reference.toText unconflicts.types.updates.bob

  Text.putStrLn (Text.bold "\n=== Alice-and-Bob unconflicts ===")
  renderUnconflicts Text.green "+" referentLabel Referent.toText unconflicts.terms.adds.both
  renderUnconflicts Text.green "+" (const "type") Reference.toText unconflicts.types.adds.both
  renderUnconflicts Text.red "-" referentLabel Referent.toText unconflicts.terms.deletes.both
  renderUnconflicts Text.red "-" (const "type") Reference.toText unconflicts.types.deletes.both
  renderUnconflicts Text.yellow "%" referentLabel Referent.toText unconflicts.terms.updates.both
  renderUnconflicts Text.yellow "%" (const "type") Reference.toText unconflicts.types.updates.both
  where
    renderConflicts :: Text -> Map Name Reference.Id -> EitherWay () -> IO ()
    renderConflicts label conflicts who =
      for_ (Map.toList conflicts) \(name, ref) ->
        Text.putStrLn $
          Text.magenta $
            "! "
              <> Text.italic label
              <> " "
              <> Name.toText name
              <> " "
              <> Reference.idToText ref
              <> " ("
              <> (case who of Alice () -> "Alice"; Bob () -> "Bob")
              <> ")"

    renderUnconflicts ::
      (Text -> Text) ->
      Text ->
      (ref -> Text) ->
      (ref -> Text) ->
      Map Name ref ->
      IO ()
    renderUnconflicts color action label renderRef unconflicts =
      for_ (Map.toList unconflicts) \(name, ref) ->
        Text.putStrLn $
          color $
            action
              <> " "
              <> Text.italic (label ref)
              <> " "
              <> Name.toText name
              <> " "
              <> renderRef ref

realDebugDependents :: TwoWay (DefnsF (Map Name) TermReferenceId TypeReferenceId) -> IO ()
realDebugDependents dependents = do
  Text.putStrLn (Text.bold "\n=== Alice dependents of Bob deletes, Bob updates, and Alice conflicts ===")
  renderThings "termid" dependents.alice.terms
  renderThings "typeid" dependents.alice.types
  Text.putStrLn (Text.bold "\n=== Bob dependents of Alice deletes, Alice updates, and Bob conflicts ===")
  renderThings "termid" dependents.bob.terms
  renderThings "typeid" dependents.bob.types
  where
    renderThings :: Text -> Map Name Reference.Id -> IO ()
    renderThings label things =
      for_ (Map.toList things) \(name, ref) ->
        Text.putStrLn $
          Text.italic label
            <> " "
            <> Name.toText name
            <> " "
            <> Reference.idToText ref

realDebugStageOne :: DefnsF (Map Name) Referent TypeReference -> IO ()
realDebugStageOne defns = do
  Text.putStrLn (Text.bold "\n=== Stage 1 ===")
  debugDefns1 defns

debugConstructorNames :: Map Name [Name] -> IO ()
debugConstructorNames names =
  for_ (Map.toList names) \(typeName, conNames) ->
    Text.putStrLn (Name.toText typeName <> " => " <> Text.intercalate ", " (map Name.toText conNames))

debugDefns1 :: DefnsF (Map Name) Referent TypeReference -> IO ()
debugDefns1 defns = do
  renderThings referentLabel Referent.toText defns.terms
  renderThings (const "type") Reference.toText defns.types
  where
    renderThings :: (ref -> Text) -> (ref -> Text) -> Map Name ref -> IO ()
    renderThings label renderRef things =
      for_ (Map.toList things) \(name, ref) ->
        Text.putStrLn (Text.italic (label ref) <> " " <> Name.toText name <> " " <> renderRef ref)

referentLabel :: Referent -> Text
referentLabel ref
  | Referent'.isConstructor ref = "constructor"
  | otherwise = "term"
