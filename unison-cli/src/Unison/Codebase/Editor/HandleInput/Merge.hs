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
import Data.Set qualified as Set
import Data.Text.IO qualified as Text
import U.Codebase.Branch.Diff (DefinitionDiffs (DefinitionDiffs), TreeDiff (TreeDiff))
import U.Codebase.Branch.Diff qualified as Diff
import U.Codebase.HashTags (CausalHash)
import U.Codebase.Reference qualified as Reference
import U.Codebase.Referent qualified as Referent
import U.Codebase.ShortHash qualified as ShortHash
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Codebase.Path (Path')
import Unison.Name (Name)
import Unison.Name qualified as Name
import Unison.NameSegment (NameSegment)
import Unison.NameSegment qualified as NameSegment
import Unison.Prelude
import Unison.Sqlite (Transaction)
import Unison.Syntax.Name qualified as Name (toText)

handleMerge :: Path' -> Path' -> Path' -> Cli ()
handleMerge alicePath0 bobPath0 resultPath = do
  alicePath <- Cli.resolvePath' alicePath0
  bobPath <- Cli.resolvePath' bobPath0
  (definitionsDiff, dependenciesDiff) <-
    Cli.runEitherTransaction do
      runExceptT do
        aliceBranch <- ExceptT (Cli.resolveAbsBranchIdV2 (Right alicePath))
        bobBranch <- ExceptT (Cli.resolveAbsBranchIdV2 (Right bobPath))
        let diff = Diff.diffBranches aliceBranch bobBranch
        diff2 <- lift (loadNamespaceDiff diff)
        pure diff2
  liftIO do
    Text.putStrLn "===== begin namespace diff ====="
    printDefinitionsDiff Nothing definitionsDiff
    Text.putStrLn "===== end namespace diff =====\n"

data DependencyDiff
  = DependencyDiff'Same
  | DependencyDiff'Diff !CausalHash !CausalHash

loadNamespaceDiff ::
  TreeDiff Transaction ->
  Transaction
    ( Cofree (Map NameSegment) DefinitionDiffs,
      Map NameSegment DependencyDiff
    )
loadNamespaceDiff (TreeDiff (diff :< Compose children)) = do
  definitionsDiff <- loadDefinitionsDiff (diff :< Compose children)
  dependenciesDiff <-
    case Map.lookup "lib" children of
      Nothing -> pure Map.empty
      Just dependencies0 -> do
        _weirdDefinitionsInLib :< Compose dependencies <- dependencies0
        -- TODO compute these
        pure Map.empty
  pure (definitionsDiff, dependenciesDiff)

loadDefinitionsDiff ::
  Cofree (Compose (Map NameSegment) Transaction) DefinitionDiffs ->
  Transaction (Cofree (Map NameSegment) DefinitionDiffs)
loadDefinitionsDiff (diff :< Compose children) = do
  children' <-
    Map.traverseWithKey
      ( \name action ->
          if name == Name.libSegment
            then do
              weirdDefinitionsInLib :< _dependencies <- action
              loadDefinitionsDiff1 (weirdDefinitionsInLib :< Compose Map.empty)
            else action >>= loadDefinitionsDiff1
      )
      children
  pure (diff :< children')

loadDefinitionsDiff1 ::
  Cofree (Compose (Map NameSegment) Transaction) DefinitionDiffs ->
  Transaction (Cofree (Map NameSegment) DefinitionDiffs)
loadDefinitionsDiff1 (diff :< Compose children) = do
  children' <- Map.traverseWithKey (\_name action -> action >>= loadDefinitionsDiff) children
  pure (diff :< children')

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
