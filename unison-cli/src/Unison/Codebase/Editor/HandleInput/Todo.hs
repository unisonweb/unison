-- | @todo@ input handler
module Unison.Codebase.Editor.HandleInput.Todo
  ( handleTodo,
  )
where

import Control.Lens hiding (from)
import Control.Monad.Reader (ask)
import Data.Set qualified as Set
import U.Codebase.Sqlite.Queries qualified as Queries
import Unison.Cli.Monad (Cli)
import Unison.Cli.Monad qualified as Cli
import Unison.Cli.MonadUtils qualified as Cli
import Unison.Cli.PrettyPrintUtils qualified as Cli
import Unison.Codebase (Codebase)
import Unison.Codebase qualified as Codebase
import Unison.Codebase.Branch.Names qualified as Branch
import Unison.Codebase.Editor.DisplayObject
import Unison.Codebase.Editor.Output
import Unison.Codebase.Editor.StructuredArgument qualified as SA
import Unison.Codebase.Editor.TodoOutput qualified as TO
import Unison.Codebase.Patch (Patch (..))
import Unison.Codebase.Patch qualified as Patch
import Unison.Codebase.Path qualified as Path
import Unison.DataDeclaration qualified as DD
import Unison.HashQualified qualified as HQ
import Unison.Names (Names)
import Unison.Names qualified as Names
import Unison.Parser.Ann (Ann (..))
import Unison.Prelude
import Unison.Reference (Reference)
import Unison.Reference qualified as Reference
import Unison.Sqlite qualified as Sqlite
import Unison.Symbol (Symbol)
import Unison.Type (Type)
import Unison.Util.Monoid qualified as Monoid
import Unison.Util.Relation qualified as R
import Unison.Util.TransitiveClosure (transitiveClosure)

handleTodo :: Maybe Path.Split' -> Path.Path' -> Cli ()
handleTodo patchPath branchPath' = do
  patch <- Cli.getPatchAt (fromMaybe Cli.defaultPatchPath patchPath)
  branchPath <- Cli.resolvePath' branchPath'
  doShowTodoOutput patch branchPath

-- | Show todo output if there are any conflicts or edits.
doShowTodoOutput :: Patch -> Path.Absolute -> Cli ()
doShowTodoOutput patch scopePath = do
  Cli.Env {codebase} <- ask
  names0 <- Branch.toNames <$> Cli.getBranch0At scopePath
  todo <- Cli.runTransaction (checkTodo codebase patch names0)
  if TO.noConflicts todo && TO.noEdits todo
    then Cli.respond NoConflictsOrEdits
    else do
      Cli.setNumberedArgs $
        SA.HashQualified . HQ.HashOnly . Reference.toShortHash . view _2
          <$> fst (TO.todoFrontierDependents todo)
      pped <- Cli.currentPrettyPrintEnvDecl
      Cli.respondNumbered $ TodoOutput pped todo

checkTodo :: Codebase m Symbol Ann -> Patch -> Names -> Sqlite.Transaction (TO.TodoOutput Symbol Ann)
checkTodo codebase patch names0 = do
  let -- Get the dependents of a reference which:
      --   1. Don't appear on the LHS of this patch
      --   2. Have a name in this namespace
      getDependents :: Reference -> Sqlite.Transaction (Set Reference)
      getDependents ref = do
        dependents <- Codebase.dependents Queries.ExcludeSelf ref
        pure (dependents & removeEditedThings & removeNamelessThings)
  -- (r,r2) ∈ dependsOn if r depends on r2, excluding self-references (i.e. (r,r))
  dependsOn <- Monoid.foldMapM (\ref -> R.fromManyDom <$> getDependents ref <*> pure ref) edited
  let dirty = R.dom dependsOn
  transitiveDirty <- transitiveClosure getDependents dirty
  (frontierTerms, frontierTypes) <- loadDisplayInfo codebase (R.ran dependsOn)
  (dirtyTerms, dirtyTypes) <- loadDisplayInfo codebase dirty
  pure $
    TO.TodoOutput
      (Set.size transitiveDirty)
      (frontierTerms, frontierTypes)
      (score dirtyTerms, score dirtyTypes)
      (Names.conflicts names0)
      (Patch.conflicts patch)
  where
    -- Remove from a all references that were edited, i.e. appear on the LHS of this patch.
    removeEditedThings :: Set Reference -> Set Reference
    removeEditedThings =
      (`Set.difference` edited)
    -- Remove all references that don't have a name in the given namespace
    removeNamelessThings :: Set Reference -> Set Reference
    removeNamelessThings =
      Set.filter (Names.contains names0)
    -- todo: something more intelligent here?
    score :: [(a, b)] -> [(TO.Score, a, b)]
    score = map (\(x, y) -> (1, x, y))
    edited :: Set Reference
    edited = R.dom (Patch._termEdits patch) <> R.dom (Patch._typeEdits patch)

loadDisplayInfo ::
  Codebase m Symbol Ann ->
  Set Reference ->
  Sqlite.Transaction
    ( [(Reference, Maybe (Type Symbol Ann))],
      [(Reference, DisplayObject () (DD.Decl Symbol Ann))]
    )
loadDisplayInfo codebase refs = do
  termRefs <- filterM (Codebase.isTerm codebase) (toList refs)
  typeRefs <- filterM (Codebase.isType codebase) (toList refs)
  terms <- forM termRefs $ \r -> (r,) <$> Codebase.getTypeOfTerm codebase r
  types <- forM typeRefs $ \r -> (r,) <$> loadTypeDisplayObject codebase r
  pure (terms, types)

loadTypeDisplayObject :: Codebase m Symbol Ann -> Reference -> Sqlite.Transaction (DisplayObject () (DD.Decl Symbol Ann))
loadTypeDisplayObject codebase = \case
  Reference.Builtin _ -> pure (BuiltinObject ())
  Reference.DerivedId id ->
    maybe (MissingObject $ Reference.idToShortHash id) UserObject
      <$> Codebase.getTypeDeclaration codebase id
