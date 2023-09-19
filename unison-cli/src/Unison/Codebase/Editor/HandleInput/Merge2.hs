-- | @merge@ input handler
module Unison.Codebase.Editor.HandleInput.Merge2
  ( handleMerge,
  )
where

import Control.Monad.Reader (ask)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Bimap (Bimap)
import Data.Bimap qualified as Bimap
import Data.ByteString.Short (ShortByteString)
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Semialign (alignWith)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Text (Builder)
import Data.Text.Lazy.Builder qualified as Text.Builder
import Data.Text.Lazy.Builder.Int qualified as Text.Builder
import Data.These (These (..))
import Data.Tuple.Strict
import GHC.Clock (getMonotonicTime)
import Text.ANSI qualified as Text
import Text.Printf (printf)
import U.Codebase.Branch (Branch, CausalBranch)
import U.Codebase.Branch qualified as Branch
import U.Codebase.Branch.Diff (DefinitionDiffs (DefinitionDiffs), Diff (..))
import U.Codebase.Branch.Diff qualified as Diff
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import U.Codebase.Reference (Reference, Reference' (..), TermReference, TermReferenceId, TypeReference, TypeReferenceId)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path (Path')
import Unison.Codebase.Path qualified as Path
import Unison.ConstructorReference (GConstructorReference (..))
import Unison.Core.ConstructorId (ConstructorId)
import Unison.DataDeclaration qualified as V1 (Decl)
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.HashQualified' qualified as HQ'
import Unison.Merge qualified as Merge
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude hiding (catMaybes)
import Unison.PrettyPrintEnv (PrettyPrintEnv (..))
import Unison.PrettyPrintEnv qualified as Ppe
import Unison.Referent qualified as V1 (Referent)
import Unison.Referent qualified as V1.Referent
import Unison.ShortHash (ShortHash)
import Unison.ShortHash qualified as ShortHash
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.SyntacticHash qualified as SyntacticHash
import Unison.Syntax.Name qualified as Name (toText)
import Unison.Term qualified as V1 (Term)
import Unison.Util.BiMultimap (BiMultimap)
import Unison.Util.BiMultimap qualified as BiMultimap
import Unison.Util.Monoid (intercalateMap)
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Unison.Util.Relation3 (Relation3)
import Unison.Util.Relation3 qualified as Relation3
import Unison.Util.Set qualified as Set
import Unison.Var (Var)
import Witherable (catMaybes)

-- Temporary simple way to time a transaction
step :: Text -> Transaction a -> Transaction a
step name action = do
  t0 <- Sqlite.unsafeIO getMonotonicTime
  result <- action
  Sqlite.unsafeIO do
    t1 <- getMonotonicTime
    Text.putStrLn (Text.pack (printf "%4d ms | " (round ((t1 - t0) * 1000) :: Int)) <> name)
  pure result

handleMerge :: Path' -> Path' -> Path' -> Cli ()
handleMerge alicePath0 bobPath0 _resultPath = do
  Cli.Env {codebase} <- ask

  alicePath <- Cli.resolvePath' alicePath0
  bobPath <- Cli.resolvePath' bobPath0

  Cli.runTransaction do
    aliceCausal <- step "load alice causal" $ Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute alicePath)
    bobCausal <- step "load bob causal" $ Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute bobPath)

    let aliceCausalHash = Causal.causalHash aliceCausal
    let bobCausalHash = Causal.causalHash bobCausal
    maybeLcaCausalHash <- step "compute lca" $ Operations.lca aliceCausalHash bobCausalHash

    -- Read the (shallow) branches out of the database.
    aliceBranch <- step "load shallow alice branch" $ Causal.value aliceCausal
    bobBranch <- step "load shallow bob branch" $ Causal.value bobCausal

    ~(Right (T2 aliceDeclNames aliceTermNames)) <- step "load alice names" $ loadBranchDefinitionNames2 aliceBranch
    ~(Right (T2 bobDeclNames bobTermNames)) <- step "load bob names" $ loadBranchDefinitionNames2 bobBranch

    let syntacticHashPpe :: PrettyPrintEnv
        syntacticHashPpe =
          -- The order isn't important here for syntactic hashing
          deepnessToPpe aliceDeclNames aliceTermNames `Ppe.addFallback` deepnessToPpe bobDeclNames bobTermNames

    aliceDeclSynhashes <- syntacticallyHashDecls (Codebase.unsafeGetTypeDeclaration codebase) syntacticHashPpe aliceDeclNames
    aliceTermSynhashes <- syntacticallyHashTerms (Codebase.unsafeGetTerm codebase) syntacticHashPpe aliceTermNames

    bobDeclSynhashes <- syntacticallyHashDecls (Codebase.unsafeGetTypeDeclaration codebase) syntacticHashPpe bobDeclNames
    bobTermSynhashes <- syntacticallyHashTerms (Codebase.unsafeGetTerm codebase) syntacticHashPpe bobTermNames

    (aliceDeclDiff, aliceTermDiff, aliceDependenciesDiff, bobDeclDiff, bobTermDiff, bobDependenciesDiff) <-
      case maybeLcaCausalHash of
        Nothing -> do
          let synhashesToAdds :: BiMultimap hash name -> Map name (Op hash)
              synhashesToAdds =
                Map.map Added . BiMultimap.range
          aliceDependenciesDiff <- loadDependenciesAdds aliceBranch
          bobDependenciesDiff <- loadDependenciesAdds bobBranch
          pure
            ( synhashesToAdds aliceDeclSynhashes,
              synhashesToAdds aliceTermSynhashes,
              aliceDependenciesDiff,
              synhashesToAdds bobDeclSynhashes,
              synhashesToAdds bobTermSynhashes,
              bobDependenciesDiff
            )
        Just lcaCausalHash -> do
          lcaCausal <- step "load lca causal" $ Operations.expectCausalBranchByCausalHash lcaCausalHash
          lcaBranch <- step "load lca shallow branch" $ Causal.value lcaCausal
          ~(Right (T2 lcaDeclNames lcaTermNames)) <- step "load lca names" $ loadBranchDefinitionNames2 lcaBranch

          lcaDeclSynhashes <- syntacticallyHashDecls (Codebase.unsafeGetTypeDeclaration codebase) syntacticHashPpe lcaDeclNames
          lcaTermSynhashes <- syntacticallyHashTerms (Codebase.unsafeGetTerm codebase) syntacticHashPpe lcaTermNames

          let aliceDeclDiff = diffish lcaDeclSynhashes aliceDeclSynhashes
          let aliceTermDiff = diffish lcaTermSynhashes aliceTermSynhashes

          let bobDeclDiff = diffish lcaDeclSynhashes bobDeclSynhashes
          let bobTermDiff = diffish lcaTermSynhashes bobTermSynhashes

          -- TODO: look at diffs, bail out if either bob or alice updated some-but-not-all names for any particular thing

          aliceDependenciesDiff <- step "load alice dependencies diff" $ loadDependenciesDiff lcaBranch aliceBranch
          bobDependenciesDiff <- step "load alice dependencies diff" $ loadDependenciesDiff lcaBranch bobBranch

          pure (aliceDeclDiff, aliceTermDiff, aliceDependenciesDiff, bobDeclDiff, bobTermDiff, bobDependenciesDiff)

    let conflictedDecls = conflictsish aliceDeclDiff bobDeclDiff
    let conflictedTerms = conflictsish aliceTermDiff bobDeclDiff

    Sqlite.unsafeIO do
      -- Text.putStrLn ""
      -- Text.putStrLn "===== hashes ====="
      -- Text.putStrLn ("alice causal hash = " <> showCausalHash aliceCausalHash)
      -- Text.putStrLn ("alice namespace hash = " <> showNamespaceHash (Causal.valueHash aliceCausal))
      -- Text.putStrLn ("bob causal hash = " <> showCausalHash bobCausalHash)
      -- Text.putStrLn ("bob namespace hash = " <> showNamespaceHash (Causal.valueHash bobCausal))
      -- Text.putStrLn ("lca causal hash = " <> showCausalHash lcaCausalHash)
      Text.putStrLn ""
      Text.putStrLn "===== lca->alice diff ====="
      printDeclsDiff aliceDeclNames aliceDeclDiff
      printTermsDiff aliceTermNames aliceTermDiff
      printDependenciesDiff aliceDependenciesDiff
      Text.putStrLn ""
      Text.putStrLn "===== lca->bob diff ====="
      printDeclsDiff bobDeclNames bobDeclDiff
      printTermsDiff bobTermNames bobTermDiff
      printDependenciesDiff bobDependenciesDiff
      Text.putStrLn ""
      Text.putStrLn "===== conflicts ====="
      printDeclConflicts conflictedDecls
      printTermConflicts conflictedTerms
      Text.putStrLn ""

      -- Text.writeFile
      --   "ec-graph.dot"
      --   ( ecDependenciesToDot
      --       (luniqRelationToRelation aliceTypeNames <> luniqRelationToRelation bobTypeNames)
      --       (aliceDataconNames <> bobDataconNames)
      --       (luniqRelationToRelation aliceTermNames <> luniqRelationToRelation bobTermNames)
      --       (Relation.ran typeUserUpdates)
      --       (Relation.ran termUserUpdates)
      --       coreEcs
      --       coreEcDependencies
      --   )
      -- Process.callCommand "dot -Tpdf ec-graph.dot > ec-graph.pdf && open ec-graph.pdf && rm ec-graph.dot"

      pure ()

-- | Load all term and type names from a branch (excluding dependencies) into memory.
--
-- Fails if:
--   * One name is associated with more than one reference.
loadBranchDefinitionNames2 ::
  forall m.
  Monad m =>
  Branch m ->
  -- TODO better failure type than text
  m (Either Text (T2 (BiMultimap TypeReference Name) (BiMultimap Referent Name)))
loadBranchDefinitionNames2 =
  runExceptT . go []
  where
    go ::
      [NameSegment] ->
      Branch m ->
      ExceptT Text m (T2 (BiMultimap TypeReference Name) (BiMultimap Referent Name))
    go reversePrefix branch = do
      types <- ExceptT (pure (branchTypeNames reversePrefix branch))
      terms <- ExceptT (pure (branchTermNames reversePrefix branch))

      T2 childrenTypes childrenTerms <-
        -- Ignore children namespaces of `lib`
        if reversePrefix == [Name.libSegment]
          then pure (T2 BiMultimap.empty BiMultimap.empty)
          else do
            childrenNames <-
              for (Map.toList (Branch.children branch)) \(childName, childCausal) -> do
                childBranch <- lift (Causal.value childCausal)
                go (childName : reversePrefix) childBranch
            -- These unions are safe because names of one child (e.g. "child1.foo.bar") can't equal the names of any other
            -- child (e.g. "child2.baz.qux").
            let combine (T2 xs0 ys0) (T2 xs1 ys1) =
                  T2 (BiMultimap.unsafeUnion xs0 xs1) (BiMultimap.unsafeUnion ys0 ys1)
            pure (foldr combine (T2 BiMultimap.empty BiMultimap.empty) childrenNames)

      -- These unions are safe because the names at this level (e.g. "foo.bar.baz") can't equal the names at deeper
      -- levels (e.g. "foo.bar.baz.qux")
      let allTypes = BiMultimap.unsafeUnion types childrenTypes
      let allTerms = BiMultimap.unsafeUnion terms childrenTerms
      pure (T2 allTypes allTerms)

    branchTypeNames :: [NameSegment] -> Branch m -> Either Text (BiMultimap TypeReference Name)
    branchTypeNames reversePrefix branch =
      branchDefnNames reversePrefix (Branch.types branch)

    branchTermNames :: [NameSegment] -> Branch m -> Either Text (BiMultimap Referent Name)
    branchTermNames reversePrefix branch =
      branchDefnNames reversePrefix (Branch.terms branch)

    branchDefnNames ::
      forall metadata ref.
      Ord ref =>
      [NameSegment] ->
      Map NameSegment (Map ref metadata) ->
      Either Text (BiMultimap ref Name)
    branchDefnNames reversePrefix =
      foldr f (Right BiMultimap.empty) . Map.toList
      where
        f ::
          (NameSegment, Map ref metadata) ->
          Either Text (BiMultimap ref Name) ->
          Either Text (BiMultimap ref Name)
        f (segment, refs) = \case
          Left err -> Left err
          Right acc ->
            case Set.asSingleton (Map.keysSet refs) of
              Nothing -> Left ("multiple refs with name " <> Name.toText name)
              Just ref -> Right (BiMultimap.insert ref name acc)
          where
            name = Name.fromReverseSegments (segment :| reversePrefix)

deepnessToPpe :: BiMultimap TypeReference Name -> BiMultimap Referent Name -> PrettyPrintEnv
deepnessToPpe typeNames termNames =
  PrettyPrintEnv
    ( \ref ->
        BiMultimap.lookupDom (referent1to2 ref) termNames
          & Set.lookupMin
          & maybe [] nameToPpeEntry
    )
    ( \ref ->
        BiMultimap.lookupDom ref typeNames
          & Set.lookupMin
          & maybe [] nameToPpeEntry
    )
  where
    -- Our pretty-print env takes V1 referents, which have constructor types, but we can safely throw those constructor
    -- types away, because the constructor reference is all we need to look up in our map.
    referent1to2 :: V1.Referent -> Referent
    referent1to2 = \case
      V1.Referent.Con (ConstructorReference typeRef conId) _conTy -> Referent.Con typeRef conId
      V1.Referent.Ref termRef -> Referent.Ref termRef

    nameToPpeEntry :: Name -> [(HQ'.HashQualified Name, HQ'.HashQualified Name)]
    nameToPpeEntry name =
      [(HQ'.NameOnly name, HQ'.NameOnly name)]

syntacticallyHashDecls ::
  (Monad m, Var v) =>
  (TypeReferenceId -> m (V1.Decl v a)) ->
  PrettyPrintEnv ->
  BiMultimap TypeReference Name ->
  m (BiMultimap Hash Name)
syntacticallyHashDecls loadDecl ppe =
  BiMultimap.unsafeTraverseDom \case
    ReferenceBuiltin name -> pure (SyntacticHash.hashBuiltinDecl name)
    ReferenceDerived ref -> do
      decl <- loadDecl ref
      pure (SyntacticHash.hashDecl ppe ref decl)

syntacticallyHashTerms ::
  (Monad m, Var v) =>
  (TermReferenceId -> m (V1.Term v a)) ->
  PrettyPrintEnv ->
  BiMultimap Referent Name ->
  m (BiMultimap Hash Name)
syntacticallyHashTerms loadTerm ppe =
  BiMultimap.unsafeTraverseDom \case
    Referent.Con _ _ -> pure hashThatIsDistinctFromAllTermHashes
    Referent.Ref (ReferenceBuiltin name) -> pure (SyntacticHash.hashBuiltinTerm name)
    Referent.Ref (ReferenceDerived ref) -> do
      term <- loadTerm ref
      pure (SyntacticHash.hashTerm ppe term)
  where
    -- TODO explain better why it's fine to give all data constructors the same syntactic hash, so long as it's
    -- different than any term hash. the skinny:
    --   * want datacon->term or vice versa to look like a conflict
    --   * datacon->datacon is fine to consider not-conflicted because a conflict, if any, would appear on the decl
    hashThatIsDistinctFromAllTermHashes :: Hash
    hashThatIsDistinctFromAllTermHashes =
      Hash.Hash (mempty :: ShortByteString)

data Op a
  = Added !a
  | Deleted !a
  | Updated !a !a

-- diffish(lca, alice)
diffish :: forall hash name. (Eq hash, Ord name) => BiMultimap hash name -> BiMultimap hash name -> Map name (Op hash)
diffish old new =
  Map.mapMaybe id (alignWith f (BiMultimap.range old) (BiMultimap.range new))
  where
    f :: These hash hash -> Maybe (Op hash)
    f = \case
      This x -> Just (Deleted x)
      That y -> Just (Added y)
      These x y
        | x == y -> Nothing
        | otherwise -> Just (Updated x y)

-- conflictsish(diffish(lca, alice), diffish(lca, bob))
conflictsish :: forall hash name. (Eq hash, Ord name) => Map name (Op hash) -> Map name (Op hash) -> Set name
conflictsish aliceDiff bobDiff =
  Map.keysSet (Map.mapMaybe id (alignWith f aliceDiff bobDiff))
  where
    f :: These (Op hash) (Op hash) -> Maybe ()
    f = \case
      These (Added x) (Added y) | x /= y -> Just ()
      These (Updated _ x) (Updated _ y) | x /= y -> Just ()
      _ -> Nothing

-- | Diff the "dependencies" ("lib" sub-namespace) of two namespaces.
loadDependenciesDiff :: Branch Transaction -> Branch Transaction -> Transaction (Map NameSegment (Op CausalHash))
loadDependenciesDiff branch1 branch2 = do
  dependencies1 <- namespaceDependencies branch1
  dependencies2 <- namespaceDependencies branch2
  pure (catMaybes (alignWith f dependencies1 dependencies2))
  where
    f = \case
      This x -> Just (Deleted (Causal.causalHash x))
      That y -> Just (Added (Causal.causalHash y))
      These (Causal.causalHash -> x) (Causal.causalHash -> y) ->
        if x == y
          then Nothing
          else Just (Updated x y)

-- | Like @loadDependenciesDiff@, but when there isn't an old and new branch to compare (i.e. no LCA), so everything
-- gets classified as an add.
loadDependenciesAdds :: Branch Transaction -> Transaction (Map NameSegment (Op CausalHash))
loadDependenciesAdds branch = do
  dependencies <- namespaceDependencies branch
  pure (Map.map (Added . Causal.causalHash) dependencies)

-- | Extract just the "dependencies" (sub-namespaces of "lib") of a branch.
namespaceDependencies :: Branch Transaction -> Transaction (Map NameSegment (CausalBranch Transaction))
namespaceDependencies branch =
  case Map.lookup Name.libSegment (Branch.children branch) of
    Nothing -> pure Map.empty
    Just dependenciesCausal -> Branch.children <$> Causal.value dependenciesCausal

-----------------------------------------------------------------------------------------------------------------------
-- Debug show/print utils

showCausalHash :: CausalHash -> Text
showCausalHash =
  ("#" <>) . Text.take 4 . Hash.toBase32HexText . unCausalHash

showNamespaceHash :: BranchHash -> Text
showNamespaceHash =
  ("#" <>) . Text.take 4 . Hash.toBase32HexText . unBranchHash

showReference :: Reference -> Text
showReference =
  showShortHash . Reference.toShortHash

showReferent :: Referent -> Text
showReferent =
  showShortHash . Referent.toShortHash

showShortHash :: ShortHash -> Text
showShortHash =
  ShortHash.toText . ShortHash.shortenTo 4

printDeclsDiff :: BiMultimap TypeReference Name -> Map Name (Op Hash) -> IO ()
printDeclsDiff declNames = do
  Text.putStr . Text.unlines . map f . Map.toList
  where
    f :: (Name, Op Hash) -> Text
    f (name, op) =
      case op of
        Added _ -> Text.green ("decl " <> Name.toText name) <> ref
        Deleted _ -> Text.red ("decl " <> Name.toText name) <> ref
        Updated _ _ -> Text.magenta ("decl " <> Name.toText name) <> ref
      where
        ref =
          Text.brightBlack (showReference (fromJust (BiMultimap.lookupRan name declNames)))

printTermsDiff :: BiMultimap Referent Name -> Map Name (Op Hash) -> IO ()
printTermsDiff termNames = do
  Text.putStr . Text.unlines . map f . Map.toList
  where
    f :: (Name, Op Hash) -> Text
    f (name, op) =
      case op of
        Added _ -> Text.green ("term " <> Name.toText name) <> ref
        Deleted _ -> Text.red ("term " <> Name.toText name) <> ref
        Updated _ _ -> Text.magenta ("decl " <> Name.toText name) <> ref
      where
        ref =
          Text.brightBlack (showReferent (fromJust (BiMultimap.lookupRan name termNames)))

printDependenciesDiff :: Map NameSegment (Op CausalHash) -> IO ()
printDependenciesDiff =
  Text.putStr . Text.unlines . map f . Map.toList
  where
    f (name, diff) =
      case diff of
        Added hash ->
          Text.green ("dependency " <> NameSegment.toText name) <> Text.brightBlack (showCausalHash hash)
        Deleted hash ->
          Text.red ("dependency " <> NameSegment.toText name) <> Text.brightBlack (showCausalHash hash)
        Updated _oldHash newHash ->
          Text.magenta ("dependency " <> NameSegment.toText name) <> Text.brightBlack (showCausalHash newHash)

printDeclConflicts :: Set Name -> IO ()
printDeclConflicts =
  Text.putStrLn . Text.unwords . map (("decl " <>) . Name.toText) . Set.toList

printTermConflicts :: Set Name -> IO ()
printTermConflicts =
  Text.putStrLn . Text.unwords . map (("term " <>) . Name.toText) . Set.toList
