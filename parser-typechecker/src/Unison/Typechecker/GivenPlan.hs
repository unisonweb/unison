-- | Resolve checker goals into a complete, position-indexed rewrite plan.
module Unison.Typechecker.GivenPlan (PlanError (..), plan) where

import Data.Map.Strict qualified as Map
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
  deriving stock (Show)

data Pending v = Pending (Map Word64 v) (Map Int (Term v Ann))

-- | Call only with notes from the successful final checking pass. Slot maps
-- make note ordering irrelevant; duplicate or incomplete decisions fail closed.
plan :: (Var v) => [Given.Given v Ann] -> [Context.InfoNote v Ann] -> Either (PlanError v) (Map SiteId (Apply.Rewrite v))
plan ambient notes = foldM add Map.empty notes >>= Map.traverseWithKey finish
  where
    add pending note = case note of
      Context.DictionaryParameter site _ v -> do
        let Pending parameters arguments = Map.findWithDefault (Pending Map.empty Map.empty) site pending
            key = Var.freshId v
        when (Map.member key parameters) (Left (DuplicateParameter site v))
        pure (Map.insert site (Pending (Map.insert key v parameters) arguments) pending)
      Context.ConstraintGoal site slot location goal givens -> do
        let Pending parameters arguments = Map.findWithDefault (Pending Map.empty Map.empty) site pending
        when (Map.member slot arguments) (Left (DuplicateArgument site slot))
        lexical <- for givens \(v, scope, typ) -> do
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
