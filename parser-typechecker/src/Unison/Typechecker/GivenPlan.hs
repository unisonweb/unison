-- | Resolve checker goals into a complete, position-indexed rewrite plan.
module Unison.Typechecker.GivenPlan (PlanError (..), plan, planWithVisibleGivens) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.Parser.Ann (Ann, SiteId)
import Unison.Prelude
import Unison.Term (Term)
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.GivenApply qualified as Apply
import Unison.Typechecker.GivenGoal qualified as Goal
import Unison.Typechecker.GivenResolver qualified as Given
import Unison.Typechecker.TypeVar qualified as TypeVar
import Unison.Var (Var)
import Unison.Var qualified as Var

data PlanError v
  = GoalFailure Ann (Goal.GoalError v Ann)
  | GivenNeedsAnnotation Ann v
  | DuplicateArgument SiteId Int
  | MissingArgument SiteId
  | DuplicateParameter SiteId v
  | DuplicateGivenBinding SiteId
  | MissingGivenBinding SiteId
  | MissingGoalScope SiteId
  deriving stock (Show)

data Pending v = Pending (Map Word64 v) (Map Int (Term v Ann))

-- | Call only with notes from the successful final checking pass. Slot maps
-- make note ordering irrelevant; duplicate or incomplete decisions fail closed.
plan :: (Var v) => [Given.Given v Ann] -> [Context.InfoNote v Ann] -> Either (PlanError v) (Map SiteId (Apply.Rewrite v))
plan = buildPlan Nothing

-- | Use source visibility and completed binding schemes, including forward
-- references inferred after a goal. Only generated parameters use live snapshots.
planWithVisibleGivens :: (Var v) => Map SiteId (Set SiteId) -> [Given.Given v Ann] -> [Context.InfoNote v Ann] -> Either (PlanError v) (Map SiteId (Apply.Rewrite v))
planWithVisibleGivens visibility = buildPlan (Just visibility)

buildPlan :: (Var v) => Maybe (Map SiteId (Set SiteId)) -> [Given.Given v Ann] -> [Context.InfoNote v Ann] -> Either (PlanError v) (Map SiteId (Apply.Rewrite v))
buildPlan visibility ambient notes = do
  completed <- foldM collect Map.empty notes
  foldM (add completed) Map.empty notes >>= Map.traverseWithKey finish
  where
    collect completed = \case
      Context.GivenBinding site _ v scope typ -> do
        when (Map.member site completed) (Left (DuplicateGivenBinding site))
        pure (Map.insert site (v, scope, typ) completed)
      _ -> pure completed
    candidates completed site captured = case visibility of
      Nothing -> pure captured
      Just scopes -> do
        visible <- maybe (Left (MissingGoalScope site)) Right (Map.lookup site scopes)
        source <- for (Set.toList visible) \binder ->
          maybe (Left (MissingGivenBinding binder)) Right (Map.lookup binder completed)
        pure (source <> [(v, scope, typ) | (v, scope@Given.Parameter {}, typ) <- captured])
    add completed pending note = case note of
      Context.DictionaryParameter site _ v -> do
        let Pending parameters arguments = Map.findWithDefault (Pending Map.empty Map.empty) site pending
            key = Var.freshId v
        when (Map.member key parameters) (Left (DuplicateParameter site v))
        pure (Map.insert site (Pending (Map.insert key v parameters) arguments) pending)
      Context.ConstraintGoal site slot location goal givens -> do
        let Pending parameters arguments = Map.findWithDefault (Pending Map.empty Map.empty) site pending
        when (Map.member slot arguments) (Left (DuplicateArgument site slot))
        available <- candidates completed site givens
        lexical <- for available \(v, scope, typ) -> do
          when (any unresolved (ABT.allVars typ)) (Left (GivenNeedsAnnotation location v))
          pure (Given.givenFromType (Given.Local v) scope (TypeVar.lowerType typ))
        tree <- first (GoalFailure location) (Goal.resolve (lexical <> ambient) goal)
        pure (Map.insert site (Pending parameters (Map.insert slot (Apply.dictionaryTerm location tree) arguments)) pending)
      _ -> pure pending
    unresolved TypeVar.Existential {} = True
    unresolved _ = False
    finish site (Pending parameters arguments)
      | Map.keys arguments /= [0 .. Map.size arguments - 1] = Left (MissingArgument site)
      | otherwise = Right (Apply.Rewrite (Map.elems parameters) (Map.elems arguments))
