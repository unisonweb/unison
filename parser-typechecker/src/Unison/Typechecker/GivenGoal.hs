-- | Keep inference variables out of dictionary search.
module Unison.Typechecker.GivenGoal (GoalError (..), resolve) where

import Data.Set qualified as Set
import Unison.ABT qualified as ABT
import Unison.Prelude
import Unison.Type (Type)
import Unison.Typechecker.GivenResolver qualified as Given
import Unison.Typechecker.TypeVar (TypeVar (..))
import Unison.Typechecker.TypeVar qualified as TypeVar
import Unison.Var (Var)

data GoalError v loc
  = AnnotationRequired (Set v)
  | ResolutionFailed (Given.ResolveError v loc)
  deriving stock (Show)

-- | The caller must first substitute the typechecker's solved context into
-- this goal, retaining unsolved existentials in the note across generalization.
-- A candidate can instantiate its own variables, but cannot determine the goal.
-- Rigid universals are preserved and remain rigid during search.
resolve :: (Var v) => [Given.Given v loc] -> Type (TypeVar b v) loc -> Either (GoalError v loc) (Given.ResolutionTree v loc)
resolve candidates goal
  | Set.null unsolved = first ResolutionFailed (Given.resolve candidates (TypeVar.lowerType goal))
  | otherwise = Left (AnnotationRequired unsolved)
  where
    unsolved = Set.fromList [v | Existential _ v <- ABT.allVars goal]
