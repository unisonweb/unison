module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,
  )
where

import Control.Lens (Lens', over, view, (%=), (.=), (.~), (^.))
import Control.Monad.Except qualified as Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.State.Strict (StateT)
import Control.Monad.State.Strict qualified as State
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Except qualified as Except
import Data.Foldable (foldlM)
import Data.Function (on)
import Data.Functor.Compose (Compose (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Merge.Strict qualified as Map
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Semialign (Semialign (..), alignWith, unzip, zip)
import Data.Set qualified as Set
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NESet
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These (These (..))
import GHC.Clock (getMonotonicTime)
import Text.Printf (printf)
import U.Codebase.Branch (Branch (..), CausalBranch)
import U.Codebase.Branch qualified as Branch
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import U.Codebase.Reference
  ( Reference,
    Reference' (..),
    ReferenceType,
    TermReference,
    TermReferenceId,
    TypeReference,
    TypeReferenceId,
  )
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.DbId (ProjectId)
import U.Codebase.Sqlite.HashHandle qualified as HashHandle
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project (Project)
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch)
import U.Codebase.Sqlite.ProjectBranch qualified as Sqlite (ProjectBranch)
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Pretty qualified as Pretty
import Unison.Cli.ProjectUtils qualified as Cli
import Unison.Cli.TypeCheck (computeTypecheckingEnvironment, typecheckTerm)
import Unison.Cli.UniqueTypeGuidLookup qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch qualified as V1 (Branch (..), Branch0)
import Unison.Codebase.Branch qualified as V1.Branch
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Causal qualified as V1 (Causal)
import Unison.Codebase.Causal qualified as V1.Causal
import Unison.Codebase.Causal.Type qualified as V1.Causal
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.HandleInput.Update2 (addDefinitionsToUnisonFile, findCtorNames, forwardCtorNames, getExistingReferencesNamed, getNamespaceDependentsOf)
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Conversions
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Merge.Database (MergeDatabase (..), makeMergeDatabase, referent2to1)
import Unison.Merge.Diff qualified as Merge
import Unison.Merge.DiffOp qualified as Merge
import Unison.Merge.Libdeps qualified as Merge
import Unison.Merge.PreconditionViolation qualified as Merge
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.NamesWithHistory qualified as NamesWithHistory
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl qualified as PPED
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.Referent qualified as V1 (Referent)
import Unison.Referent qualified as V1.Referent
import Unison.Server.Backend qualified as Backend
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Syntax.Parser qualified as Parser
import Unison.UnisonFile (TypecheckedUnisonFile, UnisonFile)
import Unison.UnisonFile qualified as UnisonFile
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Map qualified as Map
import Unison.Util.Nametree
  ( Defns (..),
    Nametree (..),
    bimapDefns,
    flattenNametree,
    traverseNametreeWithName,
    unflattenNametree,
  )
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Witch (unsafeFrom)
import Prelude hiding (unzip, zip)
import Unison.Codebase (Codebase)

-- Temporary simple way to time a transaction
step :: Text -> Transaction a -> Transaction a
step name action = do
  t0 <- Sqlite.unsafeIO getMonotonicTime
  result <- action
  Sqlite.unsafeIO do
    t1 <- getMonotonicTime
    Text.putStrLn (Text.pack (printf "%4d ms | " (round ((t1 - t0) * 1000) :: Int)) <> name)
  pure result

handleMerge :: ProjectBranchName -> Cli ()
handleMerge bobBranchName = do
  Cli.Env {codebase} <- ask
  -- Create a bunch of cached database lookup functions
  db <- do
    Cli.Env {codebase} <- ask
    makeMergeDatabase codebase

  -- Load the current project branch ("alice"), and the branch from the same project to merge in ("bob")
  mergeInfo <- getMergeInfo bobBranchName
  (conflictInfo, unisonFile, pped) <- Cli.runTransactionWithRollback \abort -> do
    conflictInfo <- getConflictInfo db mergeInfo abort
    case hasConflicts conflictInfo of
      False -> do
        -- no conflicts

        lcaNamesExcludingLibdeps <- loadLcaNamesExcludingLibdeps db conflictInfo

        let termAndDeclNames = bimapDefns Map.keysSet Map.keysSet (unconflictedDefns conflictInfo)
        unisonFile <- makeUnisonFile abort codebase lcaNamesExcludingLibdeps conflictInfo
        unconflictedNamesExcludingLibdeps <- loadUnconflictedNamesExcludingLibdeps db conflictInfo
        let namesExcludingLibdeps = lcaNamesExcludingLibdeps <> unconflictedNamesExcludingLibdeps
        let mergedLibdepNames = Branch.toNames (V1.Branch.head $ mergedLibdeps conflictInfo)
        let namesIncludingLibdeps = namesExcludingLibdeps <> mergedLibdepNames
        let pped = PPED.makePPED (PPE.namer namesIncludingLibdeps) (PPE.suffixifyByName namesIncludingLibdeps)
        pure (conflictInfo, unisonFile, pped)
      True -> do
        -- conflicts
        error "conflicts path not implemented yet"
  let prettyUf = Pretty.prettyUnisonFile pped unisonFile
  promptUser mergeInfo conflictInfo prettyUf
  pure ()

makeUnisonFile :: (forall x. Output -> Transaction x) -> Codebase IO Symbol Ann -> Names -> ConflictInfo -> Transaction (UnisonFile Symbol Ann)
makeUnisonFile abort codebase lcaNamesExcludingLibdeps conflictInfo = do
  unisonFile <- do
    addDefinitionsToUnisonFile
      abort
      codebase
      -- todo: fix output
      -- todo: not lca, we need to find all constructor names in unconflicted defns
      (findCtorNames Output.UOUUpgrade lcaNamesExcludingLibdeps (forwardCtorNames lcaNamesExcludingLibdeps))
      (unconflictedRel conflictInfo)
      UnisonFile.emptyUnisonFile

  -- dependents <-
  --   getNamespaceDependentsOf lcaNamesExcludingLibdeps (getExistingReferencesNamed termAndDeclNames lcaNamesExcludingLibdeps)
  -- unisonFile <- do
  --   addDefinitionsToUnisonFile
  --     abort
  --     codebase
  --     -- todo: fix output
  --     (findCtorNames Output.UOUUpgrade lcaNamesExcludingLibdeps (forwardCtorNames lcaNamesExcludingLibdeps))
  --     dependents
  --     unisonFile0
  pure unisonFile

data MergeInfo = MergeInfo
  { alicePath :: Path.Absolute,
    bobPath :: Path.Absolute,
    aliceProjectBranch :: ProjectBranch,
    bobProjectBranch :: ProjectBranch,
    project :: Project
  }

getMergeInfo :: ProjectBranchName -> Cli MergeInfo
getMergeInfo bobBranchName = do
  (ProjectAndBranch project aliceProjectBranch, _path) <- Cli.expectCurrentProjectBranch
  bobProjectBranch <- Cli.expectProjectBranchByName project bobBranchName
  let projectBranches = Merge.TwoWay {alice = aliceProjectBranch, bob = bobProjectBranch}
  let alicePath = Cli.projectBranchPath (ProjectAndBranch (project ^. #projectId) (aliceProjectBranch ^. #branchId))
  let bobPath = Cli.projectBranchPath (ProjectAndBranch (project ^. #projectId) (bobProjectBranch ^. #branchId))
  pure
    MergeInfo
      { alicePath,
        bobPath,
        aliceProjectBranch,
        bobProjectBranch,
        project
      }

getConflictInfo ::
  MergeDatabase ->
  MergeInfo ->
  (forall a. Output -> Transaction a) ->
  Transaction ConflictInfo
getConflictInfo
  db@MergeDatabase {loadCausal}
  MergeInfo
    { alicePath,
      bobPath,
      aliceProjectBranch,
      bobProjectBranch,
      project
    }
  abort0 = do
    let projectBranches = Merge.TwoWay {alice = aliceProjectBranch, bob = bobProjectBranch}
    -- Helper used throughout: abort this transaction with an output message.
    let abort :: Merge.PreconditionViolation -> Transaction void
        abort =
          mergePreconditionViolationToOutput db >=> abort0
    -- Load causals
    aliceCausal <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute alicePath)
    bobCausal <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute bobPath)
    maybeLcaCausal <-
      step "compute lca" (Operations.lca (Causal.causalHash aliceCausal) (Causal.causalHash bobCausal)) >>= \case
        Nothing -> pure Nothing
        Just lcaCausalHash -> do
          -- If LCA == bob, then we are at or ahead of bob, so the merge is done.
          when (lcaCausalHash == bobCausal ^. #causalHash) do
            abort0 $
              Output.MergeAlreadyUpToDate
                (Right (ProjectAndBranch project bobProjectBranch))
                (Right (ProjectAndBranch project aliceProjectBranch))
          Just <$> loadCausal lcaCausalHash
    -- Load shallow branches
    aliceBranch <- Causal.value aliceCausal
    bobBranch <- Causal.value bobCausal

    -- Load deep definitions
    --
    -- maybe todo: optimize this by getting defns from in memory root branch
    (_aliceCausalTree, _aliceDeclNames, aliceDefns) <-
      step "load alice definitions" do
        (definitions0, causalHashes) <- unzip <$> loadNamespaceInfo abort (aliceCausal ^. #causalHash) aliceBranch
        (declNames, definitions1) <- assertNamespaceSatisfiesPreconditions db abort (projectBranches ^. #alice . #name) aliceBranch definitions0
        pure (causalHashes, declNames, definitions1)
    (_bobCausalTree, _bobDeclNames, bobDefns) <-
      step "load bob definitions" do
        (definitions0, causalHashes) <- unzip <$> loadNamespaceInfo abort (bobCausal ^. #causalHash) bobBranch
        (declNames, definitions1) <- assertNamespaceSatisfiesPreconditions db abort (projectBranches ^. #bob . #name) bobBranch definitions0
        pure (causalHashes, declNames, definitions1)
    let defns = Merge.TwoWay {alice = aliceDefns, bob = bobDefns}

    (lcaDefns, lcaLibdeps, diffs) <- do
      case maybeLcaCausal of
        Nothing -> do
          diffs <-
            Merge.nameBasedNamespaceDiff
              db
              Merge.TwoOrThreeWay {lca = Nothing, alice = aliceDefns, bob = bobDefns}
          pure (Defns BiMultimap.empty BiMultimap.empty, Map.empty, diffs)
        Just lcaCausal -> do
          lcaBranch <- Causal.value lcaCausal
          lcaDefns <- loadLcaDefinitions abort (lcaCausal ^. #causalHash) lcaBranch
          diffs <-
            Merge.nameBasedNamespaceDiff
              db
              Merge.TwoOrThreeWay {lca = Just lcaDefns, alice = aliceDefns, bob = bobDefns}
          abortIfAnyConflictedAliases abort projectBranches lcaDefns diffs
          lcaLibdeps <- maybe Map.empty snd <$> loadLibdeps lcaBranch
          pure (lcaDefns, lcaLibdeps, diffs)

    -- Load and merge libdeps
    (libdepsCausalParents, libdeps) <- do
      maybeAliceLibdeps <- loadLibdeps aliceBranch
      maybeBobLibdeps <- loadLibdeps bobBranch
      pure $
        ( Set.fromList (catMaybes [fst <$> maybeAliceLibdeps, fst <$> maybeBobLibdeps]),
          Merge.mergeLibdeps
            ((==) `on` Causal.causalHash)
            getTwoFreshNames
            lcaLibdeps
            (maybe Map.empty snd maybeAliceLibdeps)
            (maybe Map.empty snd maybeBobLibdeps)
        )

    let conflictedNames =
          Defns
            { terms = getConflicts (view #terms <$> diffs),
              types = getConflicts (view #types <$> diffs)
            }
    -- TODO is swapping constructors' names handled correctly here?
    -- TODO is exchanging constructor for function handled correctly here?
    -- TODO is exchanging function for constructor handled correctly here?
    let (conflictedDefns, unconflictedDefns) = partitionConflicts conflictedNames defns
    mergedLibdeps <- convertLibdepsToV1Causal db libdepsCausalParents libdeps
    pure ConflictInfo {lcaDefns, conflictedDefns, unconflictedDefns, mergedLibdeps}

data ConflictInfo = ConflictInfo
  { conflictedDefns :: Defns (Map Name (Referent, Referent)) (Map Name (TypeReference, TypeReference)),
    unconflictedDefns :: Defns (Map Name Referent) (Map Name TypeReference),
    lcaDefns :: Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name),
    mergedLibdeps :: V1.Branch Transaction
  }

partitionConflicts ::
  -- | Conflicted names
  Defns (Set Name) (Set Name) ->
  -- | alice and bob defns
  Merge.TwoWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name)) ->
  -- | (conflicted defns, unconflicted defns)
  ( Defns (Map Name (Referent, Referent)) (Map Name (TypeReference, TypeReference)),
    Defns (Map Name Referent) (Map Name TypeReference)
  )
partitionConflicts (Defns conflictedTermNames conflictedTypeNames) Merge.TwoWay {alice = Defns aliceTerms aliceTypes, bob = Defns bobTerms bobTypes} =
  let aliceTermMap = BiMultimap.range aliceTerms
      aliceTypeMap = BiMultimap.range aliceTypes
      bobTermMap = BiMultimap.range bobTerms
      bobTypeMap = BiMultimap.range bobTypes
      (conflictedTerms, unconflictedTerms) = Map.foldlWithKey (phi conflictedTermNames) (Map.empty, Map.empty) (align aliceTermMap bobTermMap)
      (conflictedTypes, unconflictedTypes) = Map.foldlWithKey (phi conflictedTypeNames) (Map.empty, Map.empty) (align aliceTypeMap bobTypeMap)

      phi :: forall v. Set Name -> (Map Name (v, v), Map Name v) -> Name -> These v v -> (Map Name (v, v), Map Name v)
      phi conflictedNames = \(conflicted, unconflicted) k v -> case v of
        This v ->
          let !unconflicted' = Map.insert k v unconflicted
           in (conflicted, unconflicted')
        That v ->
          let !unconflicted' = Map.insert k v unconflicted
           in (conflicted, unconflicted')
        These v0 v1 -> case Set.member k conflictedNames of
          True ->
            let !conflicted' = Map.insert k (v0, v1) conflicted
             in (conflicted', unconflicted)
          False ->
            let !unconflicted' = Map.insert k v0 unconflicted
             in (conflicted, unconflicted')
   in (Defns conflictedTerms conflictedTypes, Defns unconflictedTerms unconflictedTypes)

loadNamesExcludingLibdeps :: MergeDatabase -> ConflictInfo -> Transaction Names
loadNamesExcludingLibdeps db ci = (<>) <$> loadLcaNamesExcludingLibdeps db ci <*> loadUnconflictedNamesExcludingLibdeps db ci

loadLcaNamesExcludingLibdeps :: MergeDatabase -> ConflictInfo -> Transaction Names
loadLcaNamesExcludingLibdeps db ConflictInfo {lcaDefns = Defns {terms, types}} = do
  terms <- traverse (referent2to1 db) (BiMultimap.range terms)
  pure Names.Names {terms = Relation.fromMap terms, types = Relation.fromMap (BiMultimap.range types)}

loadUnconflictedNamesExcludingLibdeps :: MergeDatabase -> ConflictInfo -> Transaction Names
loadUnconflictedNamesExcludingLibdeps db ConflictInfo {unconflictedDefns = Defns {terms, types}} = do
  terms <- traverse (referent2to1 db) terms
  pure Names.Names {terms = Relation.fromMap terms, types = Relation.fromMap types}

unconflictedRel :: ConflictInfo -> (Relation Name TermReferenceId, Relation Name TypeReferenceId)
unconflictedRel ConflictInfo {unconflictedDefns = Defns {terms, types}} =
  let termRefIds = flip mapMaybe terms \case
        Referent.Ref r -> case r of
          ReferenceBuiltin _ -> error "todo"
          ReferenceDerived r -> Just r
        Referent.Con _ _ -> Nothing
      typeRefIds =
        types <&> \case
          ReferenceBuiltin _ -> error "todo"
          ReferenceDerived r -> r
   in (Relation.fromMap termRefIds, Relation.fromMap typeRefIds)

hasConflicts :: ConflictInfo -> Bool
hasConflicts ConflictInfo {conflictedDefns} = not (null (conflictedDefns ^. #terms) && null (conflictedDefns ^. #types))

promptUser ::
  MergeInfo ->
  ConflictInfo ->
  Pretty ColorText ->
  Cli a
promptUser mergeInfo conflictInfo prettyUnisonFile = do
  Cli.Env {writeSource} <- ask
  -- let currentProjectId = currentProjectAndBranch ^. #project . #projectId
  -- let textualDescriptionOfUpgrade = "merge"
  -- -- Small race condition: since picking a branch name and creating the branch happen in different
  -- -- transactions, creating could fail.
  -- temporaryBranchName <- Cli.runTransaction (findTemporaryBranchName currentProjectId targetBranchName selfBranchName)
  -- _temporaryBranchId <-
  --   HandleInput.Branch.doCreateBranch'
  --     lcaBranch
  --     Nothing
  --     (currentProjectAndBranch ^. #project)
  --     temporaryBranchName
  --     textualDescriptionOfUpgrade
  scratchFilePath <-
    Cli.getLatestFile <&> \case
      Nothing -> "scratch.u"
      Just (file, _) -> file
  liftIO $ writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile)
  -- todo: respond with some message
  Cli.returnEarlyWithoutOutput

findTemporaryBranchName :: ProjectId -> NameSegment -> NameSegment -> Transaction ProjectBranchName
findTemporaryBranchName projectId other self = do
  Cli.findTemporaryBranchName projectId preferred
  where
    preferred :: ProjectBranchName
    preferred =
      unsafeFrom @Text $
        "merge-"
          <> NameSegment.toText other
          <> "-into-"
          <> NameSegment.toText self

-- Load namespace info into memory.
--
-- Fails if:
--   * One name is associated with more than one reference.
loadNamespaceInfo ::
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  CausalHash ->
  Branch Transaction ->
  Transaction
    ( Nametree
        ( Defns (Map NameSegment Referent) (Map NameSegment TypeReference),
          CausalHash
        )
    )
loadNamespaceInfo abort causalHash branch = do
  defns <- loadNamespaceInfo0 branch causalHash
  assertNamespaceHasNoConflictedNames defns & onLeft abort

-- | Load all "namespace definitions" of a branch, which are all terms and type declarations *except* those defined
-- in the "lib" namespace.
loadNamespaceInfo0 ::
  Monad m =>
  Branch m ->
  CausalHash ->
  m
    ( Nametree
        ( Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference)),
          CausalHash
        )
    )
loadNamespaceInfo0 branch causalHash = do
  let terms = Map.map Map.keysSet (branch ^. #terms)
  let types = Map.map Map.keysSet (branch ^. #types)
  let value = (Defns {terms, types}, causalHash)
  children <-
    for (Map.delete Name.libSegment (branch ^. #children)) \childCausal -> do
      childBranch <- Causal.value childCausal
      loadNamespaceInfo0_ childBranch (childCausal ^. #causalHash)
  pure Nametree {value, children}

loadNamespaceInfo0_ ::
  Monad m =>
  Branch m ->
  CausalHash ->
  m
    ( Nametree
        ( Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference)),
          CausalHash
        )
    )
loadNamespaceInfo0_ branch causalHash = do
  let terms = Map.map Map.keysSet (branch ^. #terms)
  let types = Map.map Map.keysSet (branch ^. #types)
  let value = (Defns {terms, types}, causalHash)
  children <-
    for (branch ^. #children) \childCausal -> do
      childBranch <- Causal.value childCausal
      loadNamespaceInfo0_ childBranch (childCausal ^. #causalHash)
  pure Nametree {value, children}

-- | Assert that there are no unconflicted names in a namespace.
assertNamespaceHasNoConflictedNames ::
  Nametree
    ( Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference)),
      CausalHash
    ) ->
  Either
    Merge.PreconditionViolation
    ( Nametree
        ( Defns (Map NameSegment Referent) (Map NameSegment TypeReference),
          CausalHash
        )
    )
assertNamespaceHasNoConflictedNames =
  traverseNametreeWithName \names (Defns {terms, types}, causalHash) -> do
    terms <-
      terms & Map.traverseWithKey \name ->
        assertUnconflicted (Merge.ConflictedTermName (Name.fromReverseSegments (name :| names)))
    types <-
      types & Map.traverseWithKey \name ->
        assertUnconflicted (Merge.ConflictedTypeName (Name.fromReverseSegments (name :| names)))
    pure (Defns terms types, causalHash)
  where
    assertUnconflicted :: (Set ref -> Merge.PreconditionViolation) -> Set ref -> Either Merge.PreconditionViolation ref
    assertUnconflicted conflicted refs =
      case Set.asSingleton refs of
        Nothing -> Left (conflicted refs)
        Just ref -> Right ref

-- Convert a merge precondition violation to an output message.
mergePreconditionViolationToOutput :: MergeDatabase -> Merge.PreconditionViolation -> Transaction Output.Output
mergePreconditionViolationToOutput db = \case
  Merge.ConflictedAliases branch name1 name2 -> pure (Output.MergeConflictedAliases branch name1 name2)
  Merge.ConflictedTermName name refs -> Output.MergeConflictedTermName name <$> Set.traverse (referent2to1 db) refs
  Merge.ConflictedTypeName name refs -> pure (Output.MergeConflictedTypeName name refs)
  Merge.ConflictInvolvingBuiltin name -> pure (Output.MergeConflictInvolvingBuiltin name)
  Merge.ConstructorAlias branch name1 name2 -> pure (Output.MergeConstructorAlias branch name1 name2)
  Merge.DefnsInLib -> pure Output.MergeDefnsInLib
  Merge.MissingConstructorName name -> pure (Output.MergeMissingConstructorName name)
  Merge.NestedDeclAlias name -> pure (Output.MergeNestedDeclAlias name)
  Merge.NoConstructorNames name -> pure (Output.MergeNoConstructorNames name)
  Merge.StrayConstructor name -> pure (Output.MergeStrayConstructor name)

-- Assert that a namespace satisfies a few preconditions.
--
-- Fails if:
--   * The "lib" namespace contains any top-level terms or decls. (Only child namespaces are expected here).
--   * Any type declarations are "incoherent" (see `checkDeclCoherency`)
assertNamespaceSatisfiesPreconditions ::
  MergeDatabase ->
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  ProjectBranchName ->
  Branch Transaction ->
  ( Nametree
      (Defns (Map NameSegment Referent) (Map NameSegment TypeReference))
  ) ->
  Transaction (Map Name [Name], Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
assertNamespaceSatisfiesPreconditions db abort branchName branch defns = do
  Map.lookup Name.libSegment (branch ^. #children) `whenJust` \libdepsCausal -> do
    libdepsBranch <- Causal.value libdepsCausal
    when (not (Map.null (libdepsBranch ^. #terms)) || not (Map.null (libdepsBranch ^. #types))) do
      abort Merge.DefnsInLib
  declNames <- checkDeclCoherency db branchName defns & onLeftM abort
  pure
    ( declNames,
      Defns
        { terms = flattenNametree (view #terms) defns,
          types = flattenNametree (view #types) defns
        }
    )

-- The "decl coherency check": a type declaration in a namespace is "coherent" if it satisfies both of the following
-- criteria.
--
--   1. For each naming of the type decl (say "Foo"#foohash), there exists exactly one name for each of its constructors
--      arbitrarily deep in the corresponding namespace ("Foo" in this example).
--
--      This allows us to render the decl naturally, as in
--
--        structural type Foo
--          = Bar Nat Int
--          | internal.hello.Bonk Nat
--
--      which corresponds to the three names
--
--        "Foo"                     => #foohash
--        "Foo.Bar"                 => #foohash#0
--        "Foo.internal.hello.Bonk" => #foohash#1
--
--      We could not do if there was at least one constructor whose full name does not contain the full name of the type
--      decl itself as a prefix.
--
--      A notable consequence of this requirement is that a second naming of a decl (i.e. an alias) cannot be embedded
--      within the first naming, as in:
--
--        type Foo = ...
--        type Foo.some.inner.namespace = ... -- an alias of Foo
--
--   2. No constructor has a "stray" name that does not have a prefix that equals the type declaration's name. For
--      example, in the namespace
--
--        "Foo"                 => #foohash
--        "Foo.Bar"             => #foohash#0
--        "Deep.What.SomeAlias" => #foohash#0
--
--      the constructor "What.SomeAlias" is "stray", as the type decl #foohash has no name that matches any prefix
--      (i.e. "Deep.What" nor "Deep").
--
-- On to the implementation. We are going to traverse the namespace depth-first. As we go, we have a stateful mapping
-- between decl reference that we *have* seen a name for in one of our parent namespace, and its corresponding set of
-- constructors that we *haven't* yet seen names for, but expect to, before fully searching the corresponding
-- sub-namespace (e.g. the child namespace named "Foo" of the namepace that declares a decl "Foo").
--
-- When processing a namespace, we first process all terms. Each constructor will fall into one of three cases:
--
--   +----------------------------------------------------------------------------------------------------------------+
--   | Case         | Mapping before       | Encountered constructor | Mapping after                                  |
--   +----------------------------------------------------------------------------------------------------------------+
--   | Happy path   | { #foo : {0, 1, 2} } | #foo#1                  | { #foo : {0, 2} }                              |
--   | Already seen | { #foo : {0, 1, 2} } | #foo#5                  | Error: duplicate naming for constructor #foo#5 |
--   | Never seen   | { #foo : {0, 1, 2} } | #bar#2                  | Error: stray constructor #bar#2                |
--   +----------------------------------------------------------------------------------------------------------------+
--
-- In "happy path", we see a naming of a constructor that we're expecting, and check it off.
-- In "already seen", we see a second naming of a constructor that we're no longer expecting, and fail.
-- In "never seen", we see a naming of a constructor before any naming of its decl, so we fail.
--
-- Next, we process all type decls. Each will again fall into one of three cases:
--
--   +-----------------------------------------------------------------------------------------------------+
--   | Case             | Mapping before       | Declaration | Num constructors | New mapping              |
--   +-----------------------------------------------------------------------------------------------------+
--   | Uninhabited decl |                      | #foo        | 0                |                          |
--   | Inhabited decl   |                      | #foo        | 1 or more        | { #foo : {0, ..., n-1} } |
--   | Already seen     | { foo : {0, 1, 2}  } | #foo        | Irrelevant       | Error: nested decl alias |
--   +-----------------------------------------------------------------------------------------------------+
--
-- In "uninhabited decl", we find a decl with no constructors, so we don't expect anything new.
-- In "already seen", we find a second naming of a decl, whose constructors will necessarily violate coherency condition
--   (1) above.
--
-- In "inhabited decl", we find a decl with N constructors, and handle it by:
--   1. Adding to our state that we expect a name for each.
--   2. Recursing into the child namespace whose name matches the decl.
--   3. (If we return from the recursion without short-circuiting) remove the mapping added in step (1) and assert that
--      its value is the empty set (meaning we encountered a name for every constructor).
--
-- Note: This check could be moved into SQLite (with sufficient schema support) some day, but for now, because the merge
-- algorithm needs to pull lots of stuff into memory anyway, we just do this in memory, too.
--
-- Note: once upon a time, decls could be "incoherent". Then, we decided we want decls to be "coherent". Thus, this
-- machinery was invented.
checkDeclCoherency ::
  MergeDatabase ->
  ProjectBranchName ->
  ( Nametree
      (Defns (Map NameSegment Referent) (Map NameSegment TypeReference))
  ) ->
  -- | Returns @Map TypeName [ConstructorName]@
  Transaction (Either Merge.PreconditionViolation (Map Name [Name]))
checkDeclCoherency MergeDatabase {loadDeclNumConstructors} branchName =
  runExceptT
    . fmap (view #declNames)
    . (`State.execStateT` DeclCoherencyCheckState Map.empty Map.empty)
    . go []
  where
    go ::
      [NameSegment] ->
      ( Nametree
          (Defns (Map NameSegment Referent) (Map NameSegment TypeReference))
      ) ->
      StateT DeclCoherencyCheckState (ExceptT Merge.PreconditionViolation Transaction) ()
    go prefix (Nametree Defns {terms, types} children) = do
      for_ (Map.toList terms) \case
        (_, Referent.Ref _) -> pure ()
        (_, Referent.Con (ReferenceBuiltin _) _) -> pure ()
        (name, Referent.Con (ReferenceDerived typeRef) conId) -> do
          DeclCoherencyCheckState {expectedConstructors} <- State.get
          expectedConstructors1 <- lift (Except.except (Map.upsertF f typeRef expectedConstructors))
          #expectedConstructors .= expectedConstructors1
          where
            f :: Maybe (IntMap MaybeConstructorName) -> Either Merge.PreconditionViolation (IntMap MaybeConstructorName)
            f = \case
              Nothing -> Left (Merge.StrayConstructor (fullName name))
              Just expected -> IntMap.alterF g (fromIntegral @Word64 @Int conId) expected
                where
                  g :: Maybe MaybeConstructorName -> Either Merge.PreconditionViolation (Maybe MaybeConstructorName)
                  g = \case
                    Nothing -> error "didnt put expected constructor id"
                    Just NoConstructorNameYet -> Right (Just (YesConstructorName (fullName name)))
                    Just (YesConstructorName firstName) -> Left (Merge.ConstructorAlias branchName firstName (fullName name))

      childrenWeWentInto <-
        forMaybe (Map.toList types) \case
          (_, ReferenceBuiltin _) -> pure Nothing
          (name, ReferenceDerived typeRef) -> do
            DeclCoherencyCheckState {expectedConstructors} <- State.get
            whatHappened <- do
              let recordNewDecl ::
                    Maybe (IntMap MaybeConstructorName) ->
                    Compose (ExceptT Merge.PreconditionViolation Transaction) WhatHappened (IntMap MaybeConstructorName)
                  recordNewDecl =
                    Compose . \case
                      Just _ -> Except.throwError (Merge.NestedDeclAlias typeName)
                      Nothing ->
                        lift (loadDeclNumConstructors typeRef) <&> \case
                          0 -> UninhabitedDecl
                          n -> InhabitedDecl (IntMap.fromAscList [(i, NoConstructorNameYet) | i <- [0 .. n - 1]])
              lift (getCompose (Map.upsertF recordNewDecl typeRef expectedConstructors))
            case whatHappened of
              UninhabitedDecl -> pure Nothing
              InhabitedDecl expectedConstructors1 -> do
                child <-
                  Map.lookup name children & onNothing do
                    Except.throwError (Merge.NoConstructorNames typeName)
                #expectedConstructors .= expectedConstructors1
                go (name : prefix) child
                DeclCoherencyCheckState {expectedConstructors} <- State.get
                -- fromJust is safe here because we upserted `typeRef` key above
                let (fromJust -> maybeConstructorNames, expectedConstructors1) =
                      Map.deleteLookup typeRef expectedConstructors
                constructorNames <-
                  unMaybeConstructorNames maybeConstructorNames & onNothing do
                    Except.throwError (Merge.MissingConstructorName typeName)
                #expectedConstructors .= expectedConstructors1
                #declNames %= Map.insert typeName constructorNames
                pure (Just name)
            where
              typeName = fullName name

      let childrenWeHaventGoneInto = children `Map.withoutKeys` Set.fromList childrenWeWentInto
      for_ (Map.toList childrenWeHaventGoneInto) \(name, child) -> go (name : prefix) child
      where
        fullName name =
          Name.fromReverseSegments (name :| prefix)

data DeclCoherencyCheckState = DeclCoherencyCheckState
  { expectedConstructors :: !(Map TypeReferenceId (IntMap MaybeConstructorName)),
    declNames :: !(Map Name [Name])
  }
  deriving stock (Generic)

data MaybeConstructorName
  = NoConstructorNameYet
  | YesConstructorName !Name

unMaybeConstructorNames :: IntMap MaybeConstructorName -> Maybe [Name]
unMaybeConstructorNames =
  traverse f . IntMap.elems
  where
    f :: MaybeConstructorName -> Maybe Name
    f = \case
      NoConstructorNameYet -> Nothing
      YesConstructorName name -> Just name

data WhatHappened a
  = UninhabitedDecl
  | InhabitedDecl !a
  deriving stock (Functor, Show)

-- Like `loadNamespaceInfo`, but for loading the LCA, which has fewer preconditions.
--
-- Fails if:
--   * One name is associated with more than one reference.
loadLcaDefinitions ::
  Monad m =>
  (forall void. Merge.PreconditionViolation -> m void) ->
  CausalHash ->
  Branch m ->
  m (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
loadLcaDefinitions abort causalHash branch = do
  defns0 <- loadNamespaceInfo0 branch causalHash
  defns1 <- assertNamespaceHasNoConflictedNames defns0 & onLeft abort
  let defns2 = fst <$> defns1
  pure
    Defns
      { terms = flattenNametree (view #terms) defns2,
        types = flattenNametree (view #types) defns2
      }

abortIfAnyConflictedAliases ::
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  Merge.TwoWay Sqlite.ProjectBranch ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  Merge.TwoWay (Defns (Map Name (Merge.DiffOp Hash)) (Map Name (Merge.DiffOp Hash))) ->
  Transaction ()
abortIfAnyConflictedAliases abort projectBranchNames lcaDefns diffs = do
  findConflictedAlias lcaDefns (diffs ^. #alice) `whenJust` \(name1, name2) ->
    abort (Merge.ConflictedAliases (projectBranchNames ^. #alice . #name) name1 name2)
  findConflictedAlias lcaDefns (diffs ^. #bob) `whenJust` \(name1, name2) ->
    abort (Merge.ConflictedAliases (projectBranchNames ^. #bob . #name) name1 name2)

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
  Defns (Map Name (Merge.DiffOp Hash)) (Map Name (Merge.DiffOp Hash)) ->
  Maybe (Name, Name)
findConflictedAlias defns diff =
  asum
    [ go (defns ^. #terms) (diff ^. #terms),
      go (defns ^. #types) (diff ^. #types)
    ]
  where
    go :: forall ref. Ord ref => BiMultimap ref Name -> Map Name (Merge.DiffOp Hash) -> Maybe (Name, Name)
    go namespace diff =
      asum (map f (Map.toList diff))
      where
        f :: (Name, Merge.DiffOp Hash) -> Maybe (Name, Name)
        f (name, op) =
          case op of
            Merge.Added _ -> Nothing
            Merge.Deleted _ -> Nothing
            Merge.Updated _ hash ->
              BiMultimap.lookupPreimage name namespace
                & Set.delete name
                & Set.toList
                & map (g hash)
                & asum
          where
            g :: Hash -> Name -> Maybe (Name, Name)
            g hash alias =
              case Map.lookup alias diff of
                Just (Merge.Updated _ hash2) | hash == hash2 -> Nothing
                _ -> Just (name, alias)

-- | Load the library dependencies (lib.*) of a namespace.
loadLibdeps :: Branch Transaction -> Transaction (Maybe (CausalHash, Map NameSegment (CausalBranch Transaction)))
loadLibdeps branch =
  case Map.lookup Name.libSegment (Branch.children branch) of
    Nothing -> pure Nothing
    Just dependenciesCausal -> do
      dependenciesBranch <- Causal.value dependenciesCausal
      pure (Just (Causal.causalHash dependenciesCausal, Branch.children dependenciesBranch))

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
      NameSegment (NameSegment.toText name0 <> "__" <> tShow i)

------------------------------------------------------------------------------------------------------------------------
-- Conflicts

-- `getConflicts diffs` returns the set of conflicted names in `diffs`, where `diffs` contains two branches' diffs from
-- their LCA.
getConflicts :: forall hash name. (Eq hash, Ord name) => Merge.TwoWay (Map name (Merge.DiffOp hash)) -> Set name
getConflicts (Merge.TwoWay aliceDiff bobDiff) =
  Map.keysSet (Map.mapMaybe id (alignWith f aliceDiff bobDiff))
  where
    f :: These (Merge.DiffOp hash) (Merge.DiffOp hash) -> Maybe ()
    f = \case
      These (Merge.Added x) (Merge.Added y) | x /= y -> Just ()
      These (Merge.Updated _ x) (Merge.Updated _ y) | x /= y -> Just ()
      -- Not a conflict:
      --   delete/delete
      -- Not a conflict, perhaps only temporarily, because it's easier to implement (we ignore these deletes):
      --   delete/update
      --   update/delete
      -- Impossible cases:
      --   add/delete
      --   add/update
      _ -> Nothing

-- `convertLibdepsToV1Causal db parents libdeps` loads `libdeps` as a V1 branch (without history), and then turns it
-- into a V1 causal using `parents` as history.
convertLibdepsToV1Causal ::
  MergeDatabase ->
  Set CausalHash ->
  Map NameSegment (CausalBranch Transaction) ->
  Transaction (V1.Branch Transaction)
convertLibdepsToV1Causal db@MergeDatabase {loadCausal, loadDeclType} parents libdeps = do
  let branch :: Branch Transaction
      branch =
        Branch
          { terms = Map.empty,
            types = Map.empty,
            patches = Map.empty,
            children = libdeps
          }

  branchHash <- HashHandle.hashBranch v2HashHandle branch

  v1Branch <- do
    -- We make a fresh branch cache to load the branch of libdeps.
    -- It would probably be better to reuse the codebase's branch cache.
    -- FIXME how slow/bad is this without that branch cache?
    branchCache <- Sqlite.unsafeIO newBranchCache
    Conversions.branch2to1 branchCache loadDeclType branch

  pure $
    addCausalHistoryV1
      db
      (HashHandle.hashCausal v2HashHandle branchHash parents)
      branchHash
      v1Branch
      (Map.fromSet loadCausal parents)

-- Add causal history to a V1 branch (V1.Branch0), making it a V1 causal (V1.Branch).
addCausalHistoryV1 ::
  MergeDatabase ->
  CausalHash ->
  BranchHash ->
  V1.Branch0 Transaction ->
  Map CausalHash (Transaction (CausalBranch Transaction)) ->
  V1.Branch Transaction
addCausalHistoryV1 MergeDatabase {loadV1Branch} currentHash valueHash0 head parents =
  V1.Branch case Map.toList parents of
    [] -> V1.Causal.UnsafeOne {currentHash, valueHash, head}
    [(parentHash, parent)] ->
      V1.Causal.UnsafeCons
        { currentHash,
          valueHash,
          head,
          tail = (parentHash, convertParent parent)
        }
    _ ->
      V1.Causal.UnsafeMerge
        { currentHash,
          valueHash,
          head,
          tails = convertParent <$> parents
        }
  where
    convertParent :: Transaction (CausalBranch Transaction) -> Transaction (V1.Causal Transaction (V1.Branch0 Transaction))
    convertParent loadParent = do
      parent <- loadParent
      v1Branch <- loadV1Branch (parent ^. #causalHash)
      pure (V1.Branch._history v1Branch)

    valueHash =
      coerce @BranchHash @(Hash.HashFor (V1.Branch0 Transaction)) valueHash0
