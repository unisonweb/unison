-- | @merge@ input handler
module Unison.Codebase.Editor.HandleInput.Merge
  ( handleMerge,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Functor.Compose (Compose (Compose))
import Data.List.NonEmpty (pattern (:|))
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These (These (..))
import Text.ANSI qualified as Text
import U.Codebase.Branch (Branch, CausalBranch)
import U.Codebase.Branch qualified as Branch
import U.Codebase.Branch.Diff (DefinitionDiffs (DefinitionDiffs), Diff (..), TreeDiff (TreeDiff))
import U.Codebase.Branch.Diff qualified as Diff
import U.Codebase.Causal qualified as Causal
import U.Codebase.Decl qualified as Decl
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import U.Codebase.Reference (Reference, Reference' (..), TermReference, TypeReference, TypeReferenceId)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent (Referent)
import U.Codebase.Referent qualified as Referent
import U.Codebase.ShortHash (ShortHash)
import U.Codebase.ShortHash qualified as ShortHash
import U.Codebase.Sqlite.Operations qualified as Operations
import U.Codebase.Sqlite.V2.HashHandle (v2HashHandle)
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path (Path')
import Unison.Codebase.Path qualified as Path
import Unison.Core.ConstructorId (ConstructorId)
import Unison.Hash qualified as Hash
import Unison.Merge qualified as Merge
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude hiding (catMaybes)
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Syntax.Name qualified as Name (toText)
import Unison.Util.Monoid (foldMapM)
import Unison.Util.Relation (Relation)
import Unison.Util.Relation3 (Relation3)
import Unison.Util.Relation3 qualified as Relation3
import Unison.Util.Relation qualified as Relation
import Witherable (catMaybes)
import U.Codebase.Decl (Decl)

handleMerge :: Path' -> Path' -> Path' -> Cli ()
handleMerge alicePath0 bobPath0 _resultPath = do
  alicePath <- Cli.resolvePath' alicePath0
  bobPath <- Cli.resolvePath' bobPath0

  Cli.runTransaction do
    aliceCausal <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute alicePath)
    bobCausal <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute bobPath)

    let aliceCausalHash = Causal.causalHash aliceCausal
    let bobCausalHash = Causal.causalHash bobCausal
    maybeLcaCausalHash <- Operations.lca aliceCausalHash bobCausalHash

    Sqlite.unsafeIO do
      Text.putStrLn "===== hashes ====="
      Text.putStrLn ("alice causal hash = " <> showCausalHash aliceCausalHash)
      Text.putStrLn ("alice namespace hash = " <> showNamespaceHash (Causal.valueHash aliceCausal))
      Text.putStrLn ("bob causal hash = " <> showCausalHash bobCausalHash)
      Text.putStrLn ("bob namespace hash = " <> showNamespaceHash (Causal.valueHash bobCausal))
      case maybeLcaCausalHash of
        Nothing -> Text.putStrLn "lca causal hash ="
        Just lcaCausalHash -> Text.putStrLn ("lca causal hash = " <> showCausalHash lcaCausalHash)
      Text.putStrLn ""

    case maybeLcaCausalHash of
      -- TODO: go down 2-way merge code paths
      Nothing -> pure ()
      Just lcaCausalHash -> do
        lcaCausal <- Operations.expectCausalBranchByCausalHash lcaCausalHash
        lcaBranch <- Causal.value lcaCausal

        aliceBranch <- Causal.value aliceCausal
        aliceDefinitionsDiff <- loadDefinitionsDiff (Diff.diffBranches lcaBranch aliceBranch)
        aliceDependenciesDiff <- loadDependenciesDiff lcaBranch aliceBranch
        let (aliceTypeUpdates, aliceTermUpdates) = definitionsDiffToUpdates aliceDefinitionsDiff

        (aliceTypeNames, aliceDataconNames, aliceTermNames) <- loadBranchDefinitionNames aliceBranch
        aliceUserTypeUpdates <-
          Merge.computeTypeUserUpdates
            v2HashHandle
            Operations.expectDeclByReference
            (\ref1 ref2 -> undefined)
            aliceTypeUpdates

        bobBranch <- Causal.value bobCausal
        bobDefinitionsDiff <- loadDefinitionsDiff (Diff.diffBranches lcaBranch bobBranch)
        bobDependenciesDiff <- loadDependenciesDiff lcaBranch bobBranch
        let (bobTypeUpdates, bobTermUpdates) = definitionsDiffToUpdates bobDefinitionsDiff

        Sqlite.unsafeIO do
          Text.putStrLn "===== lca->alice diff ====="
          printDefinitionsDiff Nothing aliceDefinitionsDiff
          printDependenciesDiff aliceDependenciesDiff
          Text.putStrLn ""
          Text.putStrLn "===== lca->bob diff ====="
          printDefinitionsDiff Nothing bobDefinitionsDiff
          printDependenciesDiff bobDependenciesDiff
          Text.putStrLn ""
          Text.putStrLn "===== alice updates ====="
          printTypeUpdates aliceTypeUpdates
          printTermUpdates aliceTermUpdates
          Text.putStrLn ""
          Text.putStrLn "===== bob updates ====="
          printTypeUpdates bobTypeUpdates
          printTermUpdates bobTermUpdates
          Text.putStrLn ""

computeConstructorMapping ::
  Relation3 Name TypeReference ConstructorId ->
  TypeReferenceId ->
  Decl v ->
  Relation3 Name TypeReference ConstructorId ->
  TypeReferenceId ->
  Decl v ->
  Maybe ([Decl.Type v] -> [Decl.Type v])
computeConstructorMapping allNames1 ref1 decl1 allNames2 ref2 decl2 = do
  -- Basic checks: there's no constructor mapping if these are clearly different types
  guard (Decl.declType decl1 == Decl.declType decl2)
  guard (Decl.modifier decl1 == Decl.modifier decl2)
  guard (length (Decl.bound decl1) == length (Decl.bound decl2))
  guard (length (Decl.constructorTypes decl1) == length (Decl.constructorTypes decl2))

  -- It all looks good so far; let's see if the data constructors' names match.
  -- TODO
  Nothing

definitionsDiffToUpdates ::
  Cofree (Map NameSegment) DefinitionDiffs ->
  (Relation Reference Reference, Relation Referent Referent)
definitionsDiffToUpdates (DefinitionDiffs {termDiffs, typeDiffs} :< children) =
  (diffToUpdates typeDiffs, diffToUpdates termDiffs) <> foldMap definitionsDiffToUpdates children

diffToUpdates :: Ord ref => Map NameSegment (Diff ref) -> Relation ref ref
diffToUpdates =
  foldMap \Diff {adds, removals} -> Relation.fromSet (Set.cartesianProduct removals adds)

-- | Load all term and type names from a branch (excluding dependencies) into memory.
loadBranchDefinitionNames ::
  forall m.
  Monad m =>
  Branch m ->
  m (Relation Name TypeReference, Relation Name (TypeReference, ConstructorId), Relation Name TermReference)
loadBranchDefinitionNames =
  go []
  where
    go ::
      [NameSegment] ->
      Branch m ->
      m (Relation Name TypeReference, Relation Name (TypeReference, ConstructorId), Relation Name TermReference)
    go reversePrefix branch = do
      let types :: Relation Name TypeReference
          types =
            Relation.fromMultimap (Map.fromList (map f (Map.toList (Branch.types branch))))
            where
              f (segment, xs) =
                (Name.fromReverseSegments (segment :| reversePrefix), Map.keysSet xs)
      let datacons :: Relation Name (TypeReference, ConstructorId)
          terms :: Relation Name TermReference
          (datacons, terms) =
            Branch.terms branch
              & Map.toList
              & foldl' f (Relation.empty, Relation.empty)
            where
              f ::
                (Relation Name (TypeReference, ConstructorId), Relation Name TermReference) ->
                (NameSegment, Map Referent metadata) ->
                (Relation Name (TypeReference, ConstructorId), Relation Name TermReference)
              f acc (!segment, !refs) =
                foldl' (g (Name.fromReverseSegments (segment :| reversePrefix))) acc (Map.keys refs)

              g ::
                Name ->
                (Relation Name (TypeReference, ConstructorId), Relation Name TermReference) ->
                Referent ->
                (Relation Name (TypeReference, ConstructorId), Relation Name TermReference)
              g name (!accDatacons, !accTerms) = \case
                Referent.Ref ref -> (accDatacons, Relation.insert name ref accTerms)
                Referent.Con ref cid -> (Relation.insert name (ref, cid) accDatacons, accTerms)
      (childrenTypes, childrenDatacons, childrenTerms) <-
        Branch.children branch
          & Map.toList
          & foldMapM \(childName, childCausal) -> do
            childBranch <- Causal.value childCausal
            go (childName : reversePrefix) childBranch
      pure
        ( types <> childrenTypes,
          datacons <> childrenDatacons,
          terms <> childrenTerms
        )

data DependencyDiff
  = AddDependency !CausalHash
  | DeleteDependency !CausalHash
  | UpdateDependency !CausalHash !CausalHash

-- | Diff the "dependencies" ("lib" sub-namespace) of two namespaces.
loadDependenciesDiff :: Branch Transaction -> Branch Transaction -> Transaction (Map NameSegment DependencyDiff)
loadDependenciesDiff branch1 branch2 = do
  dependencies1 <- namespaceDependencies branch1
  dependencies2 <- namespaceDependencies branch2
  pure (catMaybes (alignWith f dependencies1 dependencies2))
  where
    f = \case
      This dep1 -> Just (DeleteDependency (Causal.causalHash dep1))
      That dep2 -> Just (AddDependency (Causal.causalHash dep2))
      These (Causal.causalHash -> dep1) (Causal.causalHash -> dep2) ->
        if dep1 == dep2
          then Nothing
          else Just (UpdateDependency dep1 dep2)

-- | Extract just the "dependencies" (sub-namespaces of "lib") of a branch.
namespaceDependencies :: Branch Transaction -> Transaction (Map NameSegment (CausalBranch Transaction))
namespaceDependencies branch =
  case Map.lookup Name.libSegment (Branch.children branch) of
    Nothing -> pure Map.empty
    Just dependenciesCausal -> Branch.children <$> Causal.value dependenciesCausal

loadDefinitionsDiff ::
  TreeDiff Transaction ->
  Transaction (Cofree (Map NameSegment) DefinitionDiffs)
loadDefinitionsDiff (TreeDiff diff) =
  loadDefinitionsDiff1 diff

loadDefinitionsDiff1 ::
  Cofree (Compose (Map NameSegment) Transaction) DefinitionDiffs ->
  Transaction (Cofree (Map NameSegment) DefinitionDiffs)
loadDefinitionsDiff1 (diff :< Compose children0) = do
  children <-
    Map.traverseWithKey
      ( \name action ->
          if name == Name.libSegment
            then do
              weirdDefinitionsInLib :< _dependencies <- action
              loadDefinitionsDiff2 (weirdDefinitionsInLib :< Compose Map.empty)
            else action >>= loadDefinitionsDiff2
      )
      children0
  pure (diff :< children)

loadDefinitionsDiff2 ::
  Cofree (Compose (Map NameSegment) Transaction) DefinitionDiffs ->
  Transaction (Cofree (Map NameSegment) DefinitionDiffs)
loadDefinitionsDiff2 (diff :< Compose children0) = do
  children <- Map.traverseWithKey (\_name action -> action >>= loadDefinitionsDiff1) children0
  pure (diff :< children)

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

printDiff :: Maybe Name -> DefinitionDiffs -> IO ()
printDiff prefix DefinitionDiffs {termDiffs, typeDiffs} = do
  for_ (Map.toList termDiffs) \(segment, Diff.Diff {adds, removals}) -> do
    let name =
          case prefix of
            Nothing -> Name.fromSegment segment
            Just prefix1 -> Name.snoc prefix1 segment
    Text.putStrLn $
      "term "
        <> Name.toText name
        <> " "
        <> Text.unwords
          ( map (Text.red . ("-" <>) . showReferent) (Set.toList removals)
              ++ map (Text.green . ("+" <>) . showReferent) (Set.toList adds)
          )
  for_ (Map.toList typeDiffs) \(segment, Diff.Diff {adds, removals}) -> do
    let name =
          case prefix of
            Nothing -> Name.fromSegment segment
            Just prefix1 -> Name.snoc prefix1 segment
    Text.putStrLn $
      "type "
        <> Name.toText name
        <> " "
        <> Text.unwords
          ( map (Text.red . ("-" <>) . showReference) (Set.toList removals)
              ++ map (Text.green . ("+" <>) . showReference) (Set.toList adds)
          )

printDefinitionsDiff :: Maybe Name -> Cofree (Map NameSegment) DefinitionDiffs -> IO ()
printDefinitionsDiff prefix (diff :< children) = do
  printDiff prefix diff
  for_ (Map.toList children) \(segment, child) ->
    let name =
          case prefix of
            Nothing -> Name.fromSegment segment
            Just prefix1 -> Name.joinDot prefix1 (Name.fromSegment segment)
     in printDefinitionsDiff (Just name) child

printDependenciesDiff :: Map NameSegment DependencyDiff -> IO ()
printDependenciesDiff =
  Text.putStr . Text.unlines . map f . Map.toList
  where
    f (name, diff) =
      let prefix = "dep " <> NameSegment.toText name <> " "
       in case diff of
            AddDependency hash -> prefix <> plus hash
            DeleteDependency hash -> prefix <> minus hash
            UpdateDependency oldHash newHash -> prefix <> minus oldHash <> " " <> plus newHash

    plus = Text.green . ("+" <>) . showCausalHash
    minus = Text.red . ("-" <>) . showCausalHash

printTypeUpdates :: Relation Reference Reference -> IO ()
printTypeUpdates =
  Text.putStr . Text.unlines . map f . Relation.toList
  where
    f (old, new) =
      "type " <> showReference old <> " => " <> showReference new

printTermUpdates :: Relation Referent Referent -> IO ()
printTermUpdates =
  Text.putStr . Text.unlines . map f . Relation.toList
  where
    f (old, new) =
      "term " <> showReferent old <> " => " <> showReferent new
