-- | @merge@ input handler
module Unison.Codebase.Editor.HandleInput.Merge
  ( handleMerge,
  )
where

import Control.Comonad.Cofree (Cofree ((:<)))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Functor.Compose (Compose (Compose))
import Data.Functor.Identity (Identity)
import Data.Map.Strict qualified as Map
import Data.Semialign (alignWith)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Data.These (These (..))
import U.Codebase.Branch (Branch, CausalBranch)
import U.Codebase.Branch qualified as Branch
import U.Codebase.Branch.Diff (DefinitionDiffs (DefinitionDiffs), TreeDiff (TreeDiff))
import U.Codebase.Branch.Diff qualified as Diff
import U.Codebase.Causal qualified as Causal
import U.Codebase.HashTags (BranchHash (..), CausalHash (..))
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent qualified as Referent
import U.Codebase.ShortHash qualified as ShortHash
import U.Codebase.Sqlite.Operations qualified as Operations
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Path (Path')
import Unison.Codebase.Path qualified as Path
import Unison.Hash qualified as Hash
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.Sqlite (Transaction)
import Unison.Sqlite qualified as Sqlite
import Unison.Syntax.Name qualified as Name (toText)

handleMerge :: Path' -> Path' -> Path' -> Cli ()
handleMerge alicePath0 bobPath0 resultPath = do
  alicePath <- Cli.resolvePath' alicePath0
  bobPath <- Cli.resolvePath' bobPath0

  Cli.runTransaction do
    Sqlite.unsafeIO $ Text.putStrLn "===== hashes ====="

    aliceCausal <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute alicePath)
    bobCausal <- Codebase.getShallowCausalFromRoot Nothing (Path.unabsolute bobPath)

    let aliceCausalHash = Causal.causalHash aliceCausal
    let bobCausalHash = Causal.causalHash bobCausal

    Sqlite.unsafeIO $ Text.putStrLn ("alice causal hash = " <> showCausalHash aliceCausalHash)
    Sqlite.unsafeIO $ Text.putStrLn ("alice namespace hash = " <> showNamespaceHash (Causal.valueHash aliceCausal))
    Sqlite.unsafeIO $ Text.putStrLn ("bob causal hash = " <> showCausalHash bobCausalHash)
    Sqlite.unsafeIO $ Text.putStrLn ("bob namespace hash = " <> showNamespaceHash (Causal.valueHash bobCausal))

    maybeLcaCausalHash <- Operations.lca aliceCausalHash bobCausalHash

    Sqlite.unsafeIO case maybeLcaCausalHash of
      Nothing -> Text.putStrLn "lca causal hash ="
      Just lcaCausalHash -> Text.putStrLn ("lca causal hash = " <> showCausalHash lcaCausalHash)
    Sqlite.unsafeIO $ Text.putStrLn ""

    case maybeLcaCausalHash of
      -- TODO: go down 2-way merge code paths
      Nothing -> pure ()
      Just lcaCausalHash -> do
        lcaCausal <- Operations.expectCausalBranchByCausalHash lcaCausalHash
        lcaBranch <- Causal.value lcaCausal

        aliceBranch <- Causal.value aliceCausal
        let aliceDiff = Diff.diffBranches lcaBranch aliceBranch
        aliceDefinitionsDiff <- loadDefinitionsDiff aliceDiff
        Sqlite.unsafeIO do
          Text.putStrLn "===== lca->alice diff ====="
          printDefinitionsDiff Nothing aliceDefinitionsDiff
          Text.putStrLn ""

        bobBranch <- Causal.value bobCausal
        let bobDiff = Diff.diffBranches lcaBranch bobBranch
        bobDefinitionsDiff <- loadDefinitionsDiff bobDiff
        Sqlite.unsafeIO do
          Text.putStrLn "===== lca->bob diff ====="
          printDefinitionsDiff Nothing bobDefinitionsDiff
          Text.putStrLn ""

loadDependenciesDiff ::
  Branch Transaction ->
  Branch Transaction ->
  Transaction (Map NameSegment (These CausalHash CausalHash))
loadDependenciesDiff aliceBranch bobBranch = do
  aliceDependencies <- namespaceDependencies aliceBranch
  bobDependencies <- namespaceDependencies bobBranch
  pure $
    alignWith
      ( \case
          This aliceDep -> This (Causal.causalHash aliceDep)
          That bobDep -> That (Causal.causalHash bobDep)
          These aliceDep bobDep -> These (Causal.causalHash aliceDep) (Causal.causalHash bobDep)
      )
      aliceDependencies
      bobDependencies

namespaceDependencies :: Branch Transaction -> Transaction (Map NameSegment (CausalBranch Transaction))
namespaceDependencies branch =
  case Map.lookup Name.libSegment (Branch.children branch) of
    Nothing -> pure Map.empty
    Just dependenciesCausal -> do
      dependenciesNamespace <- Causal.value dependenciesCausal
      pure (Branch.children dependenciesNamespace)

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

printDefinitionsDiff :: Maybe Name -> Cofree (Map NameSegment) DefinitionDiffs -> IO ()
printDefinitionsDiff prefix (diff :< children) = do
  printDiff prefix diff
  for_ (Map.toList children) \(segment, child) ->
    let name =
          case prefix of
            Nothing -> Name.fromSegment segment
            Just prefix1 -> Name.joinDot prefix1 (Name.fromSegment segment)
     in printDefinitionsDiff (Just name) child

printDiff :: Maybe Name -> DefinitionDiffs -> IO ()
printDiff prefix DefinitionDiffs {termDiffs, typeDiffs} = do
  for_ (Map.toList termDiffs) \(segment, Diff.Diff {adds, removals}) ->
    let name =
          case prefix of
            Nothing -> Name.fromSegment segment
            Just prefix1 -> Name.joinDot prefix1 (Name.fromSegment segment)
     in if Set.null adds && Set.null removals
          then put "term" name Nothing
          else do
            for_ adds \ref -> put "term" name (Just ("+", Referent.toShortHash ref))
            for_ removals \ref -> put "term" name (Just ("-", Referent.toShortHash ref))
  for_ (Map.toList typeDiffs) \(segment, Diff.Diff {adds, removals}) ->
    let name =
          case prefix of
            Nothing -> Name.fromSegment segment
            Just prefix1 -> Name.joinDot prefix1 (Name.fromSegment segment)
     in if Set.null adds && Set.null removals
          then put "type" name Nothing
          else do
            for_ adds \ref -> put "type" name (Just ("+", Reference.toShortHash ref))
            for_ removals \ref -> put "type" name (Just ("-", Reference.toShortHash ref))
  where
    put ty name = \case
      Nothing -> Text.putStrLn (ty <> " " <> Name.toText name <> " (no changes)")
      Just (sym, hash) ->
        Text.putStrLn (ty <> " " <> Name.toText name <> " " <> sym <> ShortHash.toText (ShortHash.shortenTo 4 hash))

showCausalHash :: CausalHash -> Text
showCausalHash =
  ("#" <>) . Text.take 4 . Hash.toBase32HexText . unCausalHash

showNamespaceHash :: BranchHash -> Text
showNamespaceHash =
  ("#" <>) . Text.take 4 . Hash.toBase32HexText . unBranchHash
