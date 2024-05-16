module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,
  )
where

import Control.Lens (mapped, over, set, view, _1)
import Control.Monad.Reader (ask)
import Control.Monad.Writer (Writer)
import Control.Monad.Writer qualified as Writer
import Data.Bifoldable (bifoldMap)
import Data.Bitraversable (bitraverse)
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Semialign (align, unzip)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These (These (..))
import Text.ANSI qualified as Text
import Text.Builder qualified
import Text.Builder qualified as Text (Builder)
import U.Codebase.Branch qualified as V2 (Branch (..), CausalBranch)
import U.Codebase.Branch qualified as V2.Branch
import U.Codebase.Causal qualified as V2.Causal
import U.Codebase.HashTags (unCausalHash)
import U.Codebase.Reference (Reference, TermReferenceId, TypeReference, TypeReferenceId)
import U.Codebase.Referent qualified as V2 (Referent)
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import Unison.Builtin.Decls qualified as Builtin.Decls
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.ProjectUtils qualified as Cli
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
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Conversions
import Unison.Codebase.SqliteCodebase.Operations qualified as Operations
import Unison.ConstructorReference (ConstructorReference, GConstructorReference (..))
import Unison.DataDeclaration (Decl)
import Unison.Debug qualified as Debug
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.HashQualified qualified as HQ
import Unison.HashQualified' qualified as HQ'
import Unison.Merge.CombineDiffs (CombinedDiffOp (..), combineDiffs)
import Unison.Merge.Database (MergeDatabase (..), makeMergeDatabase, referent2to1)
import Unison.Merge.DeclCoherencyCheck (IncoherentDeclReason (..), checkDeclCoherency)
import Unison.Merge.DeclNameLookup (DeclNameLookup (..), expectConstructorNames)
import Unison.Merge.Diff qualified as Merge
import Unison.Merge.DiffOp (DiffOp (..))
import Unison.Merge.EitherWay (EitherWay (..))
import Unison.Merge.EitherWayI (EitherWayI (..))
import Unison.Merge.EitherWayI qualified as EitherWayI
import Unison.Merge.Libdeps qualified as Merge
import Unison.Merge.PartitionCombinedDiffs (partitionCombinedDiffs)
import Unison.Merge.PreconditionViolation qualified as Merge
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
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
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
import Unison.Referent' qualified as Referent'
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
import Unison.Util.Nametree (Nametree (..), flattenNametree, traverseNametreeWithName, unflattenNametree)
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Util.Star2 (Star2)
import Unison.Util.Star2 qualified as Star2
import Unison.Util.SyntaxText (SyntaxText')
import Unison.Var (Var)
import Witch (unsafeFrom)
import Prelude hiding (unzip, zip, zipWith)

handleMerge :: ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli ()
handleMerge bobSpecifier = do
  let debugFunctions =
        if Debug.shouldDebug Debug.Merge
          then realDebugFunctions
          else fakeDebugFunctions

  Cli.Env {codebase} <- ask

  -- Create a bunch of cached database lookup functions
  db <- makeMergeDatabase codebase

  -- Load the current project branch ("Alice"), and the branch from the same project to merge in ("Bob")
  info <- loadMergeInfo bobSpecifier
  let projectAndBranchNames = (\x -> ProjectAndBranch x.project.name x.branch.name) <$> info.branches

  -- Load Alice/Bob/LCA causals
  causals <-
    Cli.runTransaction do
      alice <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute info.paths.alice)
      bob <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute info.paths.bob)
      lca <-
        Operations.lca alice.causalHash bob.causalHash >>= \case
          Nothing -> pure Nothing
          Just lcaCausalHash -> Just <$> db.loadCausal lcaCausalHash
      pure TwoOrThreeWay {lca, alice, bob}

  -- If alice == bob, then we are done.
  when (causals.alice == causals.bob) do
    Cli.returnEarly (Output.MergeAlreadyUpToDate (Right info.branches.bob) (Right info.branches.alice))

  -- Otherwise, if LCA == bob, then we are ahead of bob, so we are done.
  when (causals.lca == Just causals.bob) do
    Cli.returnEarly (Output.MergeAlreadyUpToDate (Right info.branches.bob) (Right info.branches.alice))

  -- Otherwise, if LCA == alice, then we can fast forward to bob, and we're done.
  when (causals.lca == Just causals.alice) do
    bobBranch <- Cli.getBranchAt info.paths.bob
    _ <- Cli.updateAt (textualDescriptionOfMerge info) info.paths.alice (\_aliceBranch -> bobBranch)
    Cli.returnEarly (Output.MergeSuccessFastForward projectAndBranchNames.alice projectAndBranchNames.bob)

  liftIO (debugFunctions.debugCausals causals)

  -- Load Alice/Bob/LCA branches
  branches <-
    Cli.runTransaction do
      alice <- causals.alice.value
      bob <- causals.bob.value
      lca <- for causals.lca \causal -> causal.value
      pure TwoOrThreeWay {lca, alice, bob}

  -- Load Alice/Bob/LCA definitions and decl name lookups
  (defns3, declNameLookups3) <-
    Cli.runTransactionWithRollback \abort -> do
      loadDefns abort db (view #branch <$> info.branches) branches
  let defns = ThreeWay.forgetLca defns3
  let declNameLookups = ThreeWay.forgetLca declNameLookups3

  liftIO (debugFunctions.debugDefns defns3 declNameLookups3)

  -- Diff LCA->Alice and LCA->Bob
  diffs <-
    Cli.runTransaction do
      Merge.nameBasedNamespaceDiff db declNameLookups3 defns3

  liftIO (debugFunctions.debugDiffs diffs)

  -- Bail early if it looks like we can't proceed with the merge, because Alice or Bob has one or more conflicted alias
  whenJust (findOneConflictedAlias (view #branch <$> info.branches) defns3.lca diffs) \violation ->
    Cli.returnEarly (mergePreconditionViolationToOutput violation)

  -- Combine the LCA->Alice and LCA->Bob diffs together
  let diff = combineDiffs diffs

  liftIO (debugFunctions.debugCombinedDiff diff)

  -- Partition the combined diff into the conflicted things and the unconflicted things
  (conflicts, unconflicts) <-
    partitionCombinedDiffs defns declNameLookups diff & onLeft \name ->
      Cli.returnEarly (mergePreconditionViolationToOutput (Merge.ConflictInvolvingBuiltin name))

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

  let prettyUnisonFile = makePrettyUnisonFile (into @Text <$> projectAndBranchNames) renderedConflicts renderedDependents

  let stageOneBranch = defnsAndLibdepsToBranch0 codebase stageOne mergedLibdeps

  maybeTypecheckedUnisonFile <-
    let thisMergeHasConflicts =
          -- Eh, they'd either both be null, or neither, but just check both maps anyway
          not (defnsAreEmpty conflicts.alice) || not (defnsAreEmpty conflicts.bob)
     in if thisMergeHasConflicts
          then pure Nothing
          else do
            currentPath <- Cli.getCurrentPath
            parsingEnv <- makeParsingEnv currentPath (Branch.toNames stageOneBranch)
            prettyParseTypecheck2 prettyUnisonFile parsingEnv <&> eitherToMaybe

  case maybeTypecheckedUnisonFile of
    Nothing -> do
      Cli.Env {writeSource} <- ask
      aliceBranch <- Cli.getBranchAt info.paths.alice
      bobBranch <- Cli.getBranchAt info.paths.bob
      _temporaryBranchId <-
        HandleInput.Branch.doCreateBranch'
          (Branch.mergeNode stageOneBranch aliceBranch bobBranch)
          Nothing
          info.branches.alice.project
          (findTemporaryBranchName info)
          (textualDescriptionOfMerge info)
      scratchFilePath <-
        Cli.getLatestFile <&> \case
          Nothing -> "scratch.u"
          Just (file, _) -> file
      liftIO $ writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile)
      Cli.respond $
        Output.MergeFailure
          scratchFilePath
          projectAndBranchNames.alice
          projectAndBranchNames.bob
    Just tuf -> do
      Cli.runTransaction (Codebase.addDefsToCodebase codebase tuf)
      bobBranch <- Cli.getBranchAt info.paths.bob
      let stageTwoBranch = Branch.batchUpdates (typecheckedUnisonFileToBranchAdds tuf) stageOneBranch
      _ <-
        Cli.updateAt
          (textualDescriptionOfMerge info)
          info.paths.alice
          (\aliceBranch -> Branch.mergeNode stageTwoBranch aliceBranch bobBranch)
      Cli.respond (Output.MergeSuccess projectAndBranchNames.alice projectAndBranchNames.bob)

------------------------------------------------------------------------------------------------------------------------
-- Loading basic info out of the database

loadMergeInfo :: ProjectAndBranch (Maybe ProjectName) ProjectBranchName -> Cli MergeInfo
loadMergeInfo (ProjectAndBranch maybeBobProjectName bobBranchName) = do
  (aliceProjectBranch, _path) <- Cli.expectCurrentProjectBranch
  bobProjectBranch <-
    Cli.expectProjectAndBranchByTheseNames case maybeBobProjectName of
      Nothing -> That bobBranchName
      Just bobProjectName -> These bobProjectName bobBranchName
  let alicePath = Cli.projectBranchPath (ProjectAndBranch aliceProjectBranch.project.projectId aliceProjectBranch.branch.branchId)
  let bobPath = Cli.projectBranchPath (ProjectAndBranch bobProjectBranch.project.projectId bobProjectBranch.branch.branchId)
  pure
    MergeInfo
      { paths = TwoWay alicePath bobPath,
        branches = TwoWay aliceProjectBranch bobProjectBranch
      }

loadDefns ::
  (forall a. Output -> Transaction a) ->
  MergeDatabase ->
  TwoWay ProjectBranch ->
  TwoOrThreeWay (V2.Branch Transaction) ->
  Transaction
    ( ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)),
      ThreeWay DeclNameLookup
    )
loadDefns abort0 db projectBranches branches = do
  lcaDefns0 <-
    case branches.lca of
      Nothing -> pure Nametree {value = Defns Map.empty Map.empty, children = Map.empty}
      Just lcaBranch -> loadNamespaceInfo abort db lcaBranch
  aliceDefns0 <- loadNamespaceInfo abort db branches.alice
  bobDefns0 <- loadNamespaceInfo abort db branches.bob

  lca <- assertNamespaceSatisfiesPreconditions db abort Nothing (fromMaybe V2.Branch.empty branches.lca) lcaDefns0
  alice <- assertNamespaceSatisfiesPreconditions db abort (Just projectBranches.alice.name) branches.alice aliceDefns0
  bob <- assertNamespaceSatisfiesPreconditions db abort (Just projectBranches.bob.name) branches.bob bobDefns0

  pure (unzip ThreeWay {lca, alice, bob})
  where
    abort :: Merge.PreconditionViolation -> Transaction void
    abort =
      abort0 . mergePreconditionViolationToOutput

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
  Var v =>
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
      if TwoWay.or (not . defnsAreEmpty <$> dependents)
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
    go :: Ord v => Map Name v -> Nametree (Map NameSegment v)
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

data MergeInfo = MergeInfo
  { paths :: !(TwoWay Path.Absolute),
    branches :: !(TwoWay (ProjectAndBranch Project ProjectBranch))
  }
  deriving stock (Generic)

textualDescriptionOfMerge :: MergeInfo -> Text
textualDescriptionOfMerge info =
  let bobBranchText = into @Text (ProjectAndBranch info.branches.bob.project.name info.branches.bob.branch.name)
   in "merge " <> bobBranchText

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
            let f :: Foldable t => t Reference.Id -> Set Reference
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

findTemporaryBranchName :: MergeInfo -> Transaction ProjectBranchName
findTemporaryBranchName info = do
  Cli.findTemporaryBranchName info.branches.alice.project.projectId preferred
  where
    preferred :: ProjectBranchName
    preferred =
      unsafeFrom @Text $
        "merge-"
          <> mangle info.branches.bob.branch.name
          <> "-into-"
          <> mangle info.branches.alice.branch.name

    mangle :: ProjectBranchName -> Text
    mangle =
      Text.Builder.run . mangleB

    mangleB :: ProjectBranchName -> Text.Builder
    mangleB name =
      case classifyProjectBranchName name of
        ProjectBranchNameKind'Contributor user name1 -> Text.Builder.text user <> Text.Builder.char '-' <> mangleB name1
        ProjectBranchNameKind'DraftRelease semver -> "releases-drafts-" <> mangleSemver semver
        ProjectBranchNameKind'Release semver -> "releases-" <> mangleSemver semver
        ProjectBranchNameKind'NothingSpecial -> Text.Builder.text (into @Text name)

    mangleSemver :: Semver -> Text.Builder
    mangleSemver (Semver x y z) =
      Text.Builder.decimal x
        <> Text.Builder.char '.'
        <> Text.Builder.decimal y
        <> Text.Builder.char '.'
        <> Text.Builder.decimal z

-- Load namespace info into memory.
--
-- Fails if:
--   * One name is associated with more than one reference.
loadNamespaceInfo ::
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  MergeDatabase ->
  V2.Branch Transaction ->
  Transaction (Nametree (DefnsF (Map NameSegment) Referent TypeReference))
loadNamespaceInfo abort db branch = do
  defns <- loadNamespaceInfo0 (referent2to1 db) branch
  assertNamespaceHasNoConflictedNames defns & onLeft abort

-- | Load all "namespace definitions" of a branch, which are all terms and type declarations *except* those defined
-- in the "lib" namespace.
loadNamespaceInfo0 ::
  Monad m =>
  (V2.Referent -> m Referent) ->
  V2.Branch m ->
  m (Nametree (DefnsF2 (Map NameSegment) Set Referent TypeReference))
loadNamespaceInfo0 referent2to1 branch = do
  terms <-
    branch.terms
      & Map.map Map.keysSet
      & traverse (Set.traverse referent2to1)
  let types = Map.map Map.keysSet branch.types
  children <-
    for (Map.delete NameSegment.libSegment branch.children) \childCausal -> do
      childBranch <- childCausal.value
      loadNamespaceInfo0_ referent2to1 childBranch
  pure Nametree {value = Defns {terms, types}, children}

loadNamespaceInfo0_ ::
  (Monad m) =>
  (V2.Referent -> m Referent) ->
  V2.Branch m ->
  m (Nametree (DefnsF2 (Map NameSegment) Set Referent TypeReference))
loadNamespaceInfo0_ referent2to1 branch = do
  terms <-
    branch.terms
      & Map.map Map.keysSet
      & traverse (Set.traverse referent2to1)
  let types = Map.map Map.keysSet branch.types
  children <-
    for branch.children \childCausal -> do
      childBranch <- childCausal.value
      loadNamespaceInfo0_ referent2to1 childBranch
  pure Nametree {value = Defns {terms, types}, children}

-- | Assert that there are no unconflicted names in a namespace.
assertNamespaceHasNoConflictedNames ::
  Nametree (DefnsF2 (Map NameSegment) Set Referent TypeReference) ->
  Either Merge.PreconditionViolation (Nametree (DefnsF (Map NameSegment) Referent TypeReference))
assertNamespaceHasNoConflictedNames =
  traverseNametreeWithName \names defns -> do
    terms <-
      defns.terms & Map.traverseWithKey \name ->
        assertUnconflicted (Merge.ConflictedTermName (Name.fromReverseSegments (name :| names)))
    types <-
      defns.types & Map.traverseWithKey \name ->
        assertUnconflicted (Merge.ConflictedTypeName (Name.fromReverseSegments (name :| names)))
    pure Defns {terms, types}
  where
    assertUnconflicted :: (Set ref -> Merge.PreconditionViolation) -> Set ref -> Either Merge.PreconditionViolation ref
    assertUnconflicted conflicted refs =
      case Set.asSingleton refs of
        Nothing -> Left (conflicted refs)
        Just ref -> Right ref

-- Convert a merge precondition violation to an output message.
mergePreconditionViolationToOutput :: Merge.PreconditionViolation -> Output.Output
mergePreconditionViolationToOutput = \case
  Merge.ConflictedAliases branch name1 name2 -> Output.MergeConflictedAliases branch name1 name2
  Merge.ConflictedTermName name refs -> Output.MergeConflictedTermName name refs
  Merge.ConflictedTypeName name refs -> Output.MergeConflictedTypeName name refs
  Merge.ConflictInvolvingBuiltin name -> Output.MergeConflictInvolvingBuiltin name
  Merge.ConstructorAlias maybeBranch name1 name2 -> Output.MergeConstructorAlias maybeBranch name1 name2
  Merge.DefnsInLib -> Output.MergeDefnsInLib
  Merge.MissingConstructorName name -> Output.MergeMissingConstructorName name
  Merge.NestedDeclAlias shorterName longerName -> Output.MergeNestedDeclAlias shorterName longerName
  Merge.StrayConstructor name -> Output.MergeStrayConstructor name

-- Assert that a namespace satisfies a few preconditions.
--
-- Fails if:
--   * The "lib" namespace contains any top-level terms or decls. (Only child namespaces are expected here).
--   * Any type declarations are "incoherent" (see `checkDeclCoherency`)
assertNamespaceSatisfiesPreconditions ::
  MergeDatabase ->
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  Maybe ProjectBranchName ->
  V2.Branch Transaction ->
  Nametree (DefnsF (Map NameSegment) Referent TypeReference) ->
  Transaction (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name), DeclNameLookup)
assertNamespaceSatisfiesPreconditions db abort maybeBranchName branch defns = do
  whenJust (Map.lookup NameSegment.libSegment branch.children) \libdepsCausal -> do
    libdepsBranch <- libdepsCausal.value
    when (not (Map.null libdepsBranch.terms) || not (Map.null libdepsBranch.types)) do
      abort Merge.DefnsInLib

  declNameLookup <-
    checkDeclCoherency db.loadDeclNumConstructors defns
      & onLeftM (abort . incoherentDeclReasonToMergePreconditionViolation)

  pure
    ( Defns
        { terms = flattenNametree (view #terms) defns,
          types = flattenNametree (view #types) defns
        },
      declNameLookup
    )
  where
    incoherentDeclReasonToMergePreconditionViolation :: IncoherentDeclReason -> Merge.PreconditionViolation
    incoherentDeclReasonToMergePreconditionViolation = \case
      IncoherentDeclReason'ConstructorAlias firstName secondName ->
        Merge.ConstructorAlias maybeBranchName firstName secondName
      IncoherentDeclReason'MissingConstructorName name -> Merge.MissingConstructorName name
      IncoherentDeclReason'NestedDeclAlias shorterName longerName -> Merge.NestedDeclAlias shorterName longerName
      IncoherentDeclReason'StrayConstructor name -> Merge.StrayConstructor name

findOneConflictedAlias ::
  TwoWay ProjectBranch ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  TwoWay (DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference) ->
  Maybe Merge.PreconditionViolation
findOneConflictedAlias projectBranchNames lcaDefns diffs =
  aliceConflictedAliases <|> bobConflictedAliases
  where
    aliceConflictedAliases =
      findConflictedAlias lcaDefns diffs.alice <&> \(name1, name2) ->
        Merge.ConflictedAliases projectBranchNames.alice.name name1 name2

    bobConflictedAliases =
      findConflictedAlias lcaDefns diffs.bob <&> \(name1, name2) ->
        Merge.ConflictedAliases projectBranchNames.bob.name name1 name2

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
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  DefnsF3 (Map Name) DiffOp Synhashed Referent TypeReference ->
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
      ThreeWay DeclNameLookup ->
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
  ThreeWay DeclNameLookup ->
  IO ()
realDebugDefns defns declNameLookups = do
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
  renderUnconflicts Text.green "+" referentLabel Referent.toText unconflicts.terms.adds.alice (OnlyAlice ())
  renderUnconflicts Text.green "+" (const "type") Reference.toText unconflicts.types.adds.alice (OnlyAlice ())
  renderUnconflicts Text.red "-" referentLabel Referent.toText unconflicts.terms.deletes.alice (OnlyAlice ())
  renderUnconflicts Text.red "-" (const "type") Reference.toText unconflicts.types.deletes.alice (OnlyAlice ())
  renderUnconflicts Text.yellow "%" referentLabel Referent.toText unconflicts.terms.updates.alice (OnlyAlice ())
  renderUnconflicts Text.yellow "%" (const "type") Reference.toText unconflicts.types.updates.alice (OnlyAlice ())

  Text.putStrLn (Text.bold "\n=== Bob unconflicts ===")
  renderUnconflicts Text.green "+" referentLabel Referent.toText unconflicts.terms.adds.bob (OnlyBob ())
  renderUnconflicts Text.green "+" (const "type") Reference.toText unconflicts.types.adds.bob (OnlyBob ())
  renderUnconflicts Text.red "-" referentLabel Referent.toText unconflicts.terms.deletes.bob (OnlyBob ())
  renderUnconflicts Text.red "-" (const "type") Reference.toText unconflicts.types.deletes.bob (OnlyBob ())
  renderUnconflicts Text.yellow "%" referentLabel Referent.toText unconflicts.terms.updates.bob (OnlyBob ())
  renderUnconflicts Text.yellow "%" (const "type") Reference.toText unconflicts.types.updates.bob (OnlyBob ())

  Text.putStrLn (Text.bold "\n=== Alice-and-Bob unconflicts ===")
  renderUnconflicts Text.green "+" referentLabel Referent.toText unconflicts.terms.adds.both (AliceAndBob ())
  renderUnconflicts Text.green "+" (const "type") Reference.toText unconflicts.types.adds.both (AliceAndBob ())
  renderUnconflicts Text.red "-" referentLabel Referent.toText unconflicts.terms.deletes.both (AliceAndBob ())
  renderUnconflicts Text.red "-" (const "type") Reference.toText unconflicts.types.deletes.both (AliceAndBob ())
  renderUnconflicts Text.yellow "%" referentLabel Referent.toText unconflicts.terms.updates.both (AliceAndBob ())
  renderUnconflicts Text.yellow "%" (const "type") Reference.toText unconflicts.types.updates.both (AliceAndBob ())
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
      EitherWayI () ->
      IO ()
    renderUnconflicts color action label renderRef unconflicts who =
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
              <> " ("
              <> (case who of OnlyAlice () -> "Alice"; OnlyBob () -> "Bob"; AliceAndBob () -> "Alice and Bob")
              <> ")"

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
