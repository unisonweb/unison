-- | @merge@ input handler
module Unison.Codebase.Editor.HandleInput.Merge
  ( handleMerge,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Data.Functor.Compose (Compose (Compose))
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
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import U.Codebase.Reference (Reference)
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
import Unison.Util.Relation (Relation)
import Unison.Util.Relation qualified as Relation
import Witherable (catMaybes)

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

        -- aliceUserTypeUpdates <-
        --   Merge.computeTypeUserUpdates
        --     v2HashHandle
        --     undefined
        --     (\_ _ -> undefined)
        --     aliceTypeUpdates

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

definitionsDiffToUpdates ::
  Cofree (Map NameSegment) DefinitionDiffs ->
  (Relation Reference Reference, Relation Referent Referent)
definitionsDiffToUpdates (DefinitionDiffs {termDiffs, typeDiffs} :< children) =
  (diffToUpdates typeDiffs, diffToUpdates termDiffs) <> foldMap definitionsDiffToUpdates children

diffToUpdates :: Ord ref => Map NameSegment (Diff ref) -> Relation ref ref
diffToUpdates =
  foldMap \Diff {adds, removals} -> Relation.fromSet (Set.cartesianProduct removals adds)

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
            Just prefix1 -> Name.joinDot prefix1 (Name.fromSegment segment)
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
            Just prefix1 -> Name.joinDot prefix1 (Name.fromSegment segment)
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
