{-# LANGUAGE OverloadedRecordDot #-}

module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,
  )
where

import Control.Lens (view, (%~))
import Control.Monad.Reader (ask)
import Data.Function (on)
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Semialign (Semialign (..), alignWith)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.These (These (..))
import U.Codebase.Branch qualified as V2 (Branch (..), CausalBranch)
import U.Codebase.Branch qualified as V2.Branch
import U.Codebase.Causal qualified as V2.Causal
import U.Codebase.Reference (Reference, Reference' (..), TermReferenceId, TypeReference, TypeReferenceId)
import U.Codebase.Referent qualified as V2 (Referent)
import U.Codebase.Sqlite.DbId (ProjectId)
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.Project (Project (..))
import U.Codebase.Sqlite.ProjectBranch (ProjectBranch (..))
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.Pretty qualified as Pretty
import Unison.Cli.ProjectUtils qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch (Branch0)
import Unison.Codebase.Branch qualified as Branch
import Unison.Codebase.Branch.DeclCoherencyCheck (IncoherentDeclReason (..), checkDeclCoherency)
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.HandleInput.Branch qualified as HandleInput.Branch
import Unison.Codebase.Editor.HandleInput.Update2
  ( addDefinitionsToUnisonFile,
    getExistingReferencesNamed,
    getNamespaceDependentsOf,
    makeParsingEnv,
    prettyParseTypecheck,
    typecheckedUnisonFileToBranchUpdates,
  )
import Unison.Codebase.Editor.Output (Output)
import Unison.Codebase.Editor.Output qualified as Output
import Unison.Codebase.Path qualified as Path
import Unison.Codebase.SqliteCodebase.Branch.Cache (newBranchCache)
import Unison.Codebase.SqliteCodebase.Conversions qualified as Conversions
import Unison.Merge.Database (MergeDatabase (..), makeMergeDatabase, referent2to1)
import Unison.Merge.Diff qualified as Merge
import Unison.Merge.DiffOp qualified as Merge
import Unison.Merge.Libdeps qualified as Merge
import Unison.Merge.PreconditionViolation qualified as Merge
import Unison.Merge.Synhashed (Synhashed (..))
import Unison.Merge.ThreeWay (ThreeWay (..))
import Unison.Merge.TwoWay (TwoWay (..))
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment (..))
import Unison.NameSegment qualified as NameSegment
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann)
import Unison.Prelude
import Unison.PrettyPrintEnv.Names qualified as PPE
import Unison.PrettyPrintEnvDecl.Names qualified as PPED
import Unison.Project (ProjectAndBranch (..), ProjectBranchName)
import Unison.Referent (Referent)
import Unison.Referent qualified as Referent
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.UnisonFile (UnisonFile)
import Unison.UnisonFile qualified as UnisonFile
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Defns (Defns (..), bimapDefns)
import Unison.Util.Map qualified as Map (insertLookup)
import Unison.Util.Nametree (Nametree (..), flattenNametree, traverseNametreeWithName, unflattenNametree)
import Unison.Util.Pretty (ColorText, Pretty)
import Unison.Util.Pretty qualified as Pretty
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Set qualified as Set
import Unison.Util.Star2 qualified as Star2
import Witch (unsafeFrom)
import Prelude hiding (unzip, zip)

handleMerge :: ProjectBranchName -> Cli ()
handleMerge bobBranchName = do
  Cli.Env {codebase} <- ask

  -- Create a bunch of cached database lookup functions
  db <- makeMergeDatabase codebase

  -- Load the current project branch ("alice"), and the branch from the same project to merge in ("bob")
  mergeInfo <- getMergeInfo bobBranchName

  (unisonFile, lcaBranch, droppedNames) <-
    Cli.runTransactionWithRollback \abort -> do
      conflictInfo <- getConflictInfo abort db mergeInfo
      case conflictState conflictInfo of
        Conflicted _ -> do
          error "conflicts path not implemented yet"
        Unconflicted unconflictedInfo -> do
          contents <- partitionFileContents conflictInfo
          let (droppedNames, lcaNametree) =
                let bonk ::
                      Ord ref =>
                      Map Name ref ->
                      Set Name ->
                      BiMultimap ref Name ->
                      (Map Name ref, Nametree (Map NameSegment ref))
                    bonk addsAndUpdates deletes bimulti =
                      let (replaced, r0) = performAddsAndUpdates addsAndUpdates bimulti
                          (deleted, r1) = performDeletes deletes r0
                          r2 = unflattenNametree (BiMultimap.fromRange r1)
                       in (replaced <> deleted, r2)
                    (droppedTerms, terms) = bonk contents.lcaAddsAndUpdates.terms contents.lcaDeletions.terms conflictInfo.defns.lca.terms
                    (droppedTypes, types) = bonk contents.lcaAddsAndUpdates.types contents.lcaDeletions.types conflictInfo.defns.lca.types
                    droppedNames = defnsToNames (Defns droppedTerms droppedTypes)
                 in (droppedNames, Defns {terms, types})
          let lcaBranch =
                lcaNametree
                  & mergeDefnsNametree
                  & v1NametreeToBranch0
                  & Branch.setChildBranch NameSegment.libSegment (Branch.one conflictInfo.mergedLibdeps)
                  & Branch.transform0 (Codebase.runTransaction codebase)
          unisonFile <- makeUnisonFile contents.fileContents abort codebase unconflictedInfo.declNames
          pure (unisonFile, lcaBranch, droppedNames)

  let lcaNames = Branch.toNames lcaBranch
  let ppedNames = lcaNames <> droppedNames
  let pped = PPED.makePPED (PPE.namer ppedNames) (PPE.suffixifyByName ppedNames)
  let prettyUf = Pretty.prettyUnisonFile pped unisonFile
  currentPath <- Cli.getCurrentPath
  parsingEnv <- makeParsingEnv currentPath lcaNames
  prettyParseTypecheck unisonFile pped parsingEnv >>= \case
    Left prettyError -> do
      promptUser mergeInfo (Pretty.prettyUnisonFile pped unisonFile) lcaBranch
    Right tuf -> do
      mergedBranch0 <- Cli.runTransactionWithRollback \abort -> do
        updates <- typecheckedUnisonFileToBranchUpdates abort undefined tuf
        let mergedBranch0 = Branch.batchUpdates updates lcaBranch
        pure mergedBranch0
      Cli.stepAt
        (textualDescriptionOfMerge mergeInfo)
        ( Path.unabsolute mergeInfo.paths.alice,
          const mergedBranch0
        )

performAddsAndUpdates :: Map Name ref -> BiMultimap ref Name -> (Map Name ref, Map Name ref)
performAddsAndUpdates addsAndUpdates refs =
  Map.foldlWithKey'
    ( \(replaced, b) k v ->
        let (mreplacedVal, !newMap) = Map.insertLookup k v b
            !replaced' = case mreplacedVal of
              Nothing -> replaced
              Just v -> Map.insert k v replaced
         in (replaced', newMap)
    )
    (Map.empty, BiMultimap.range refs)
    addsAndUpdates

performDeletes :: Set Name -> Map Name ref -> (Map Name ref, Map Name ref)
performDeletes deletions refs =
  Map.partitionWithKey (\k _ -> not $ Set.member k deletions) refs

mergeDefnsNametree ::
  Defns (Nametree (Map NameSegment Referent)) (Nametree (Map NameSegment TypeReference)) ->
  Nametree (Map NameSegment Referent, Map NameSegment TypeReference)
mergeDefnsNametree Defns {terms, types} = alignWith phi terms types
  where
    phi = \case
      This a -> (a, Map.empty)
      That b -> (Map.empty, b)
      These a b -> (a, b)

v1NametreeToBranch0 :: forall m. Nametree (Map NameSegment Referent, Map NameSegment TypeReference) -> Branch0 m
v1NametreeToBranch0 nt =
  let starTerms =
        Star2.Star2
          { fact = Relation.dom termRel,
            d1 = termRel,
            d2 = Relation.empty
          }
      (termRel, typeRel) = bimap (Relation.swap . Relation.fromMap) (Relation.swap . Relation.fromMap) nt.value
      starTypes =
        Star2.Star2
          { fact = Relation.dom typeRel,
            d1 = typeRel,
            d2 = Relation.empty
          }
      ntChildren = Branch.one . v1NametreeToBranch0 <$> nt.children
      res = Branch.branch0 starTerms starTypes ntChildren Map.empty
   in res

makeUnisonFile ::
  Defns (Relation Name TermReferenceId) (Relation Name TypeReferenceId) ->
  (forall x. Output -> Transaction x) ->
  Codebase IO Symbol Ann ->
  Map Name [Name] ->
  Transaction (UnisonFile Symbol Ann)
makeUnisonFile Defns {terms, types} abort codebase declMap = do
  let lookupCons k = case Map.lookup k declMap of
        Nothing -> Left (error ("failed to find: " <> show k <> " in the declMap"))
        Just x -> Right x
  unisonFile <- do
    addDefinitionsToUnisonFile
      abort
      codebase
      -- todo: fix output
      (const lookupCons)
      (terms, types)
      UnisonFile.emptyUnisonFile
  pure unisonFile

data MergeInfo = MergeInfo
  { paths :: !(TwoWay Path.Absolute),
    projectBranches :: !(TwoWay ProjectBranch),
    project :: !Project
  }
  deriving stock (Generic)

textualDescriptionOfMerge :: MergeInfo -> Text
textualDescriptionOfMerge mergeInfo =
  let bobBranchText = into @Text (ProjectAndBranch mergeInfo.project.name mergeInfo.projectBranches.bob.name)
   in "merge-" <> bobBranchText

getMergeInfo :: ProjectBranchName -> Cli MergeInfo
getMergeInfo bobBranchName = do
  (ProjectAndBranch project aliceProjectBranch, _path) <- Cli.expectCurrentProjectBranch
  bobProjectBranch <- Cli.expectProjectBranchByName project bobBranchName
  let alicePath = Cli.projectBranchPath (ProjectAndBranch project.projectId aliceProjectBranch.branchId)
  let bobPath = Cli.projectBranchPath (ProjectAndBranch project.projectId bobProjectBranch.branchId)
  pure
    MergeInfo
      { paths = TwoWay alicePath bobPath,
        projectBranches = TwoWay aliceProjectBranch bobProjectBranch,
        project
      }

getConflictInfo :: (forall a. Output -> Transaction a) -> MergeDatabase -> MergeInfo -> Transaction ConflictInfo
getConflictInfo abort0 db info = do
  -- Load causals
  aliceCausal <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute info.paths.alice)
  bobCausal <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute info.paths.bob)
  maybeLcaCausal <-
    Operations.lca aliceCausal.causalHash bobCausal.causalHash >>= \case
      Nothing -> pure Nothing
      Just lcaCausalHash -> do
        -- If LCA == bob, then we are at or ahead of bob, so the merge is done.
        when (lcaCausalHash == bobCausal.causalHash) do
          abort0 $
            Output.MergeAlreadyUpToDate
              (Right (ProjectAndBranch info.project info.projectBranches.bob))
              (Right (ProjectAndBranch info.project info.projectBranches.alice))
        Just <$> db.loadCausal lcaCausalHash

  -- Load shallow branches
  branches <- do
    alice <- aliceCausal.value
    bob <- bobCausal.value
    pure TwoWay {alice, bob}
  maybeLcaBranch <- for maybeLcaCausal \causal -> causal.value

  -- Load definitions
  (declNames, defns) <- do
    lcaDefns <-
      case maybeLcaBranch of
        Nothing -> pure (Defns BiMultimap.empty BiMultimap.empty)
        Just lcaBranch -> loadLcaDefinitions abort (referent2to1 db) lcaBranch
    aliceDefns0 <- loadNamespaceInfo abort db branches.alice
    (aliceDeclNames, aliceDefns1) <-
      assertNamespaceSatisfiesPreconditions db abort info.projectBranches.alice.name branches.alice aliceDefns0
    bobDefns0 <- loadNamespaceInfo abort db branches.bob
    (bobDeclNames, bobDefns1) <-
      assertNamespaceSatisfiesPreconditions db abort info.projectBranches.bob.name branches.bob bobDefns0
    pure
      ( TwoWay {alice = aliceDeclNames, bob = bobDeclNames},
        ThreeWay {lca = lcaDefns, alice = aliceDefns1, bob = bobDefns1}
      )

  -- Diff definitions, and bail if there are any conflicted aliases, which we can't currently handle.
  diffs <- Merge.nameBasedNamespaceDiff db defns
  abortIfAnyConflictedAliases abort info.projectBranches defns.lca diffs

  -- Load and merge libdeps
  mergedLibdeps <- do
    lca <-
      case maybeLcaBranch of
        Nothing -> pure Map.empty
        Just lcaBranch -> loadLibdeps lcaBranch
    alice <- loadLibdeps branches.alice
    bob <- loadLibdeps branches.bob
    ThreeWay {lca, alice, bob}
      & Merge.mergeLibdeps ((==) `on` V2.Causal.causalHash) getTwoFreshNames
      & libdepsToBranch0 db

  let classifiedDiff :: Defns (Map Name (TwoDiffsOp Referent)) (Map Name (TwoDiffsOp TypeReference))
      classifiedDiff =
        Defns
          { terms = partitionDiff (view #terms <$> diffs),
            types = partitionDiff (view #types <$> diffs)
          }

  -- TODO is swapping constructors' names handled correctly here?
  -- TODO is exchanging constructor for function handled correctly here?
  -- TODO is exchanging function for constructor handled correctly here?
  let PartitionedDefns {unconflictedPartitionedDefns, conflicts} = partitionConflicts classifiedDiff

  let conflictState =
        case Map.null conflicts.terms && Map.null conflicts.types of
          True ->
            Unconflicted
              UnconflictedInfo
                { declNames = declNames.alice <> declNames.bob
                }
          False ->
            Conflicted
              ConflictedInfo
                { conflictedDefns = conflicts,
                  declNames
                }

  pure
    ConflictInfo
      { defns,
        conflictState,
        mergedLibdeps,
        unconflictedPartitionedDefns
      }
  where
    -- Helper used throughout: abort this transaction with an output message.
    abort :: Merge.PreconditionViolation -> Transaction void
    abort =
      abort0 . mergePreconditionViolationToOutput

data ConflictInfo = ConflictInfo
  { defns :: !(ThreeWay (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))),
    mergedLibdeps :: !(Branch0 Transaction),
    unconflictedPartitionedDefns :: !UnconflictedPartitionedDefns,
    conflictState :: !ConflictState
  }
  deriving stock (Generic)

data ConflictState
  = Conflicted !ConflictedInfo
  | Unconflicted !UnconflictedInfo

data ConflictedInfo = ConflictedInfo
  { conflictedDefns :: !(Defns (Map Name (Referent, Referent)) (Map Name (TypeReference, TypeReference))),
    declNames :: !(TwoWay (Map Name [Name]))
  }

data UnconflictedInfo = UnconflictedInfo
  { declNames :: !(Map Name [Name])
  }
  deriving stock (Generic)

data UnconflictedPartitionedDefns = UnconflictedPartitionedDefns
  { aliceAdditions :: Defns (Map Name Referent) (Map Name TypeReference),
    bobAdditions :: Defns (Map Name Referent) (Map Name TypeReference),
    bothAdditions :: Defns (Map Name Referent) (Map Name TypeReference),
    aliceUpdates :: Defns (Map Name Referent) (Map Name TypeReference),
    bobUpdates :: Defns (Map Name Referent) (Map Name TypeReference),
    bothUpdates :: Defns (Map Name Referent) (Map Name TypeReference),
    aliceDeletions :: Defns (Map Name Referent) (Map Name TypeReference),
    bobDeletions :: Defns (Map Name Referent) (Map Name TypeReference),
    bothDeletions :: Defns (Map Name Referent) (Map Name TypeReference)
  }

data PartitionedContents = PartitionedContents
  { fileContents :: Defns (Relation Name TermReferenceId) (Relation Name TypeReferenceId),
    lcaAddsAndUpdates :: Defns (Map Name Referent) (Map Name TypeReference),
    lcaDeletions :: Defns (Set Name) (Set Name)
  }

partitionFileContents :: ConflictInfo -> Transaction PartitionedContents
partitionFileContents conflictInfo = do
  let UnconflictedPartitionedDefns
        { aliceUpdates,
          bobUpdates,
          bothUpdates,
          aliceAdditions,
          bobAdditions,
          bothAdditions,
          aliceDeletions,
          bobDeletions,
          bothDeletions
        } = conflictInfo.unconflictedPartitionedDefns
  let aliceNames = defnsToNames (bimapDefns BiMultimap.range BiMultimap.range conflictInfo.defns.alice)
  let bobNames = defnsToNames (bimapDefns BiMultimap.range BiMultimap.range conflictInfo.defns.bob)
  let alicesReferences = foldMap (defnRefs bobNames) [aliceUpdates, aliceDeletions]
  let bobsReferences = foldMap (defnRefs aliceNames) [bobUpdates, bobDeletions]
  d0 <- (\(a, b) -> Defns {terms = Relation.domain a, types = Relation.domain b}) <$> getNamespaceDependentsOf aliceNames bobsReferences
  d1 <- (\(a, b) -> Defns {terms = Relation.domain a, types = Relation.domain b}) <$> getNamespaceDependentsOf bobNames alicesReferences
  let fileContentsMap = d0 <> d1
  let fileContents = bimapDefns Relation.fromMultimap Relation.fromMultimap fileContentsMap
  let lcaAddsAndUpdates =
        let candidates = aliceUpdates <> bobUpdates <> bothUpdates <> aliceAdditions <> bobAdditions <> bothAdditions
         in bimapDefns (Map.\\ fileContentsMap.terms) (Map.\\ fileContentsMap.types) candidates
  let lcaDeletions = bimapDefns Map.keysSet Map.keysSet (aliceDeletions <> bobDeletions <> bothDeletions)
  pure
    PartitionedContents
      { fileContents,
        lcaAddsAndUpdates,
        lcaDeletions
      }
  where
    defnRefs :: Names -> Defns (Map Name Referent) (Map Name TypeReference) -> Set Reference
    defnRefs n = flip getExistingReferencesNamed n . defnNames

    defnNames :: Defns (Map Name Referent) (Map Name TypeReference) -> Defns (Set Name) (Set Name)
    defnNames = bimapDefns Map.keysSet Map.keysSet

unconflictedAdditions :: UnconflictedPartitionedDefns -> Defns (Map Name Referent) (Map Name TypeReference)
unconflictedAdditions UnconflictedPartitionedDefns {aliceAdditions, bobAdditions, bothAdditions} =
  aliceAdditions <> bobAdditions <> bothAdditions

unconflictedUpdates :: UnconflictedPartitionedDefns -> Defns (Map Name Referent) (Map Name TypeReference)
unconflictedUpdates UnconflictedPartitionedDefns {aliceUpdates, bobUpdates, bothUpdates} =
  aliceUpdates <> bobUpdates <> bothUpdates

unconflictedDeletions :: UnconflictedPartitionedDefns -> Defns (Map Name Referent) (Map Name TypeReference)
unconflictedDeletions UnconflictedPartitionedDefns {aliceDeletions, bobDeletions, bothDeletions} =
  aliceDeletions <> bobDeletions <> bothDeletions

data PartitionedDefns = PartitionedDefns
  { unconflictedPartitionedDefns :: UnconflictedPartitionedDefns,
    conflicts :: Defns (Map Name (Referent, Referent)) (Map Name (TypeReference, TypeReference))
  }

data PartitionState v = PartitionState
  { aliceAdditions :: !(Map Name v),
    bobAdditions :: !(Map Name v),
    bothAdditions :: !(Map Name v),
    aliceUpdates :: !(Map Name v),
    bobUpdates :: !(Map Name v),
    bothUpdates :: !(Map Name v),
    aliceDeletions :: !(Map Name v),
    bobDeletions :: !(Map Name v),
    bothDeletions :: !(Map Name v),
    conflicts :: !(Map Name (v, v))
  }
  deriving stock (Generic)

emptyPartitionState :: PartitionState v
emptyPartitionState =
  PartitionState
    { aliceAdditions = Map.empty,
      bobAdditions = Map.empty,
      bothAdditions = Map.empty,
      aliceUpdates = Map.empty,
      bobUpdates = Map.empty,
      bothUpdates = Map.empty,
      aliceDeletions = Map.empty,
      bobDeletions = Map.empty,
      bothDeletions = Map.empty,
      conflicts = Map.empty
    }

partitionConflicts ::
  Defns (Map Name (TwoDiffsOp Referent)) (Map Name (TwoDiffsOp TypeReference)) ->
  PartitionedDefns
partitionConflicts defns =
  let termSt = Map.foldlWithKey phi emptyPartitionState defns.terms
      typeSt = Map.foldlWithKey phi emptyPartitionState defns.types

      phi :: forall v. PartitionState v -> Name -> TwoDiffsOp v -> PartitionState v
      phi = \st k -> \case
        Conflict a b -> st & #conflicts %~ Map.insert k (a, b)
        Addition actor a ->
          let l = case actor of
                Alice -> #aliceAdditions
                Bob -> #bobAdditions
                Both -> #bothAdditions
           in st & l %~ Map.insert k a
        Update actor a ->
          let l = case actor of
                Alice -> #aliceUpdates
                Bob -> #bobUpdates
                Both -> #bothUpdates
           in st & l %~ Map.insert k a
        Deletion actor a ->
          let l = case actor of
                Alice -> #aliceDeletions
                Bob -> #bobDeletions
                Both -> #bothDeletions
           in st & l %~ Map.insert k a
   in PartitionedDefns
        { unconflictedPartitionedDefns =
            UnconflictedPartitionedDefns
              { aliceAdditions = Defns termSt.aliceAdditions typeSt.aliceAdditions,
                bobAdditions = Defns termSt.bobAdditions typeSt.bobAdditions,
                bothAdditions = Defns termSt.bothAdditions typeSt.bothAdditions,
                aliceUpdates = Defns termSt.aliceUpdates typeSt.aliceUpdates,
                bobUpdates = Defns termSt.bobUpdates typeSt.bobUpdates,
                bothUpdates = Defns termSt.bothUpdates typeSt.bothUpdates,
                aliceDeletions = Defns termSt.aliceDeletions typeSt.aliceDeletions,
                bobDeletions = Defns termSt.bobDeletions typeSt.bobDeletions,
                bothDeletions = Defns termSt.bothDeletions typeSt.bothDeletions
              },
          conflicts = Defns termSt.conflicts typeSt.conflicts
        }

defnsToNames :: Defns (Map Name Referent) (Map Name TypeReference) -> Names
defnsToNames Defns {terms, types} =
  Names.Names
    { terms = Relation.fromMap terms,
      types = Relation.fromMap types
    }

defnsToRel ::
  Defns (Map Name Referent) (Map Name TypeReference) ->
  (Relation Name TermReferenceId, Relation Name TypeReferenceId)
defnsToRel Defns {terms, types} =
  let termRefIds = flip mapMaybe terms \case
        Referent.Ref r -> case r of
          ReferenceBuiltin _ -> Nothing -- todo - explode
          ReferenceDerived r -> Just r
        Referent.Con _ _ -> Nothing -- todo - get the decl
      typeRefIds =
        flip mapMaybe types \case
          ReferenceBuiltin _ -> Nothing -- todo - explode
          ReferenceDerived r -> Just r
   in (Relation.fromMap termRefIds, Relation.fromMap typeRefIds)

unconflictedRel :: ConflictInfo -> (Relation Name TermReferenceId, Relation Name TypeReferenceId)
unconflictedRel ConflictInfo {unconflictedPartitionedDefns} =
  let d = unconflictedAdditions unconflictedPartitionedDefns <> unconflictedUpdates unconflictedPartitionedDefns
   in defnsToRel d

promptUser ::
  MergeInfo ->
  Pretty ColorText ->
  Branch0 IO ->
  Cli a
promptUser mergeInfo prettyUnisonFile newBranch = do
  Cli.Env {writeSource} <- ask
  let currentProjectId = mergeInfo.project.projectId
  let targetBranchName = mergeInfo.projectBranches.bob.name
  let selfBranchName = mergeInfo.projectBranches.alice.name
  -- Small race condition: since picking a branch name and creating the branch happen in different
  -- transactions, creating could fail.
  temporaryBranchName <- Cli.runTransaction (findTemporaryBranchName currentProjectId targetBranchName selfBranchName)
  _temporaryBranchId <-
    HandleInput.Branch.doCreateBranch'
      (Branch.one newBranch)
      Nothing
      mergeInfo.project
      temporaryBranchName
      (textualDescriptionOfMerge mergeInfo)
  scratchFilePath <-
    Cli.getLatestFile <&> \case
      Nothing -> "scratch.u"
      Just (file, _) -> file
  liftIO $ writeSource (Text.pack scratchFilePath) (Text.pack $ Pretty.toPlain 80 prettyUnisonFile)
  -- todo: respond with some message
  Cli.returnEarlyWithoutOutput

findTemporaryBranchName :: ProjectId -> ProjectBranchName -> ProjectBranchName -> Transaction ProjectBranchName
findTemporaryBranchName projectId other self = do
  Cli.findTemporaryBranchName projectId preferred
  where
    preferred :: ProjectBranchName
    preferred =
      unsafeFrom @Text $
        "merge-"
          <> into @Text other
          <> "-into-"
          <> into @Text self

-- Load namespace info into memory.
--
-- Fails if:
--   * One name is associated with more than one reference.
loadNamespaceInfo ::
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  MergeDatabase ->
  V2.Branch Transaction ->
  Transaction (Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference)))
loadNamespaceInfo abort db branch = do
  defns <- loadNamespaceInfo0 (referent2to1 db) branch
  assertNamespaceHasNoConflictedNames defns & onLeft abort

-- | Load all "namespace definitions" of a branch, which are all terms and type declarations *except* those defined
-- in the "lib" namespace.
loadNamespaceInfo0 ::
  Monad m =>
  (V2.Referent -> m Referent) ->
  V2.Branch m ->
  m (Nametree (Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference))))
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
  Monad m =>
  (V2.Referent -> m Referent) ->
  V2.Branch m ->
  m (Nametree (Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference))))
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
  Nametree (Defns (Map NameSegment (Set Referent)) (Map NameSegment (Set TypeReference))) ->
  Either Merge.PreconditionViolation (Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference)))
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
  Merge.ConstructorAlias branch name1 name2 -> Output.MergeConstructorAlias branch name1 name2
  Merge.DefnsInLib -> Output.MergeDefnsInLib
  Merge.MissingConstructorName name -> Output.MergeMissingConstructorName name
  Merge.NestedDeclAlias name -> Output.MergeNestedDeclAlias name
  Merge.NoConstructorNames name -> Output.MergeNoConstructorNames name
  Merge.StrayConstructor name -> Output.MergeStrayConstructor name

-- Assert that a namespace satisfies a few preconditions.
--
-- Fails if:
--   * The "lib" namespace contains any top-level terms or decls. (Only child namespaces are expected here).
--   * Any type declarations are "incoherent" (see `checkDeclCoherency`)
assertNamespaceSatisfiesPreconditions ::
  MergeDatabase ->
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  ProjectBranchName ->
  V2.Branch Transaction ->
  (Nametree (Defns (Map NameSegment Referent) (Map NameSegment TypeReference))) ->
  Transaction (Map Name [Name], Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
assertNamespaceSatisfiesPreconditions db abort branchName branch defns = do
  Map.lookup NameSegment.libSegment branch.children `whenJust` \libdepsCausal -> do
    libdepsBranch <- libdepsCausal.value
    when (not (Map.null libdepsBranch.terms) || not (Map.null libdepsBranch.types)) do
      abort Merge.DefnsInLib
  declNames <-
    checkDeclCoherency db.loadDeclNumConstructors defns
      & onLeftM (abort . incoherentDeclReasonToMergePreconditionViolation)
  pure
    ( declNames,
      Defns
        { terms = flattenNametree (view #terms) defns,
          types = flattenNametree (view #types) defns
        }
    )
  where
    incoherentDeclReasonToMergePreconditionViolation :: IncoherentDeclReason -> Merge.PreconditionViolation
    incoherentDeclReasonToMergePreconditionViolation = \case
      IncoherentDeclReason'ConstructorAlias firstName secondName ->
        Merge.ConstructorAlias branchName firstName secondName
      IncoherentDeclReason'MissingConstructorName name -> Merge.MissingConstructorName name
      IncoherentDeclReason'NestedDeclAlias name -> Merge.NestedDeclAlias name
      IncoherentDeclReason'NoConstructorNames name -> Merge.NoConstructorNames name
      IncoherentDeclReason'StrayConstructor name -> Merge.StrayConstructor name

-- Like `loadNamespaceInfo`, but for loading the LCA, which has fewer preconditions.
--
-- Fails if:
--   * One name is associated with more than one reference.
loadLcaDefinitions ::
  Monad m =>
  (forall void. Merge.PreconditionViolation -> m void) ->
  (V2.Referent -> m Referent) ->
  V2.Branch m ->
  m (Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name))
loadLcaDefinitions abort referent2to1 branch = do
  defns0 <- loadNamespaceInfo0 referent2to1 branch
  defns1 <- assertNamespaceHasNoConflictedNames defns0 & onLeft abort
  pure
    Defns
      { terms = flattenNametree (view #terms) defns1,
        types = flattenNametree (view #types) defns1
      }

abortIfAnyConflictedAliases ::
  (forall void. Merge.PreconditionViolation -> Transaction void) ->
  TwoWay ProjectBranch ->
  Defns (BiMultimap Referent Name) (BiMultimap TypeReference Name) ->
  TwoWay (Defns (Map Name (Merge.DiffOp (Synhashed Referent))) (Map Name (Merge.DiffOp (Synhashed TypeReference)))) ->
  Transaction ()
abortIfAnyConflictedAliases abort projectBranchNames lcaDefns diffs = do
  whenJust (findConflictedAlias lcaDefns diffs.alice) \(name1, name2) ->
    abort (Merge.ConflictedAliases projectBranchNames.alice.name name1 name2)
  whenJust (findConflictedAlias lcaDefns diffs.bob) \(name1, name2) ->
    abort (Merge.ConflictedAliases projectBranchNames.bob.name name1 name2)

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
  Defns (Map Name (Merge.DiffOp (Synhashed Referent))) (Map Name (Merge.DiffOp (Synhashed TypeReference))) ->
  Maybe (Name, Name)
findConflictedAlias defns diff =
  asum [go defns.terms diff.terms, go defns.types diff.types]
  where
    go :: forall ref. Ord ref => BiMultimap ref Name -> Map Name (Merge.DiffOp (Synhashed ref)) -> Maybe (Name, Name)
    go namespace diff =
      asum (map f (Map.toList diff))
      where
        f :: (Name, Merge.DiffOp (Synhashed ref)) -> Maybe (Name, Name)
        f (name, op) =
          case op of
            Merge.Added _ -> Nothing
            Merge.Deleted _ -> Nothing
            Merge.Updated _ hashed1 ->
              BiMultimap.lookupPreimage name namespace
                & Set.delete name
                & Set.toList
                & map (g hashed1)
                & asum
          where
            g :: Synhashed ref -> Name -> Maybe (Name, Name)
            g hashed1 alias =
              case Map.lookup alias diff of
                Just (Merge.Updated _ hashed2) | hashed1 == hashed2 -> Nothing
                _ -> Just (name, alias)

-- | Load the library dependencies (lib.*) of a namespace.
loadLibdeps :: V2.Branch Transaction -> Transaction (Map NameSegment (V2.CausalBranch Transaction))
loadLibdeps branch =
  case Map.lookup NameSegment.libSegment branch.children of
    Nothing -> pure Map.empty
    Just libdepsCausal -> do
      libdepsBranch <- libdepsCausal.value
      pure libdepsBranch.children

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

data TwoDiffsOp v
  = Conflict v v
  | Addition Actor v
  | Update Actor v -- new value
  | Deletion Actor v -- old value

data Actor
  = Alice
  | Bob
  | Both

------------------------------------------------------------------------------------------------------------------------
-- Conflicts

-- `getConflicts diffs` returns the set of conflicted names in `diffs`, where `diffs` contains two branches' diffs from
-- their LCA.
partitionDiff :: TwoWay (Map Name (Merge.DiffOp (Synhashed v))) -> Map Name (TwoDiffsOp v)
partitionDiff diffs =
  alignWith (f Alice Bob) diffs.alice diffs.bob
  where
    diffOpToTag :: Actor -> Merge.DiffOp (Synhashed v) -> TwoDiffsOp v
    diffOpToTag actor = \case
      Merge.Added x -> Addition actor x.value
      Merge.Updated _ x -> Update actor x.value
      Merge.Deleted x -> Deletion actor x.value

    f :: Actor -> Actor -> These (Merge.DiffOp (Synhashed v)) (Merge.DiffOp (Synhashed v)) -> TwoDiffsOp v
    f this that = \case
      These (Merge.Added x) (Merge.Added y) -> if x /= y then Conflict x.value y.value else Addition Both x.value
      These (Merge.Added _) (Merge.Updated _ _) -> error "impossible"
      These (Merge.Added _) (Merge.Deleted _) -> error "impossible"
      These (Merge.Updated _ x) (Merge.Updated _ y) -> if x /= y then Conflict x.value y.value else Update Both x.value
      -- Not a conflict, perhaps only temporarily, because it's easier to implement (we ignore these deletes):
      These (Merge.Updated _ x) (Merge.Deleted _) -> Update this x.value
      These (Merge.Updated _ _) (Merge.Added _) -> error "impossible"
      These (Merge.Deleted x) (Merge.Deleted _) -> Deletion Both x.value
      These a@(Merge.Deleted _) b -> f that this (These b a)
      This x -> diffOpToTag this x
      That x -> diffOpToTag that x

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
