module Unison.Test.Typechecker.GivenGoal (test) where

import Data.Set qualified as Set
import EasyTest
import Unison.ABT qualified as ABT
import Unison.Symbol (Symbol)
import Unison.Type qualified as Type
import Unison.Typechecker.GivenGoal qualified as Goal
import Unison.Typechecker.GivenResolver qualified as Given
import Unison.Typechecker.TypeVar (TypeVar (..))
import Unison.Typechecker.TypeVar qualified as TV
import Unison.Var qualified as Var

test :: Test ()
test =
  let a = Var.named "a" :: Symbol
      b = Var.named "b"
      dictionary = Var.named "dictionary"
      candidate = Given.Given (Given.Local dictionary) [] [] (Type.nat ()) Given.Ambient
      unknown v = Type.var () (Existential () v)
      rigid v = Type.var () (Universal v) :: Type.Type (TypeVar () Symbol) ()
      required typ vars = case Goal.resolve [candidate] typ of
        Left (Goal.AnnotationRequired actual) -> expectEqual actual (Set.fromList vars)
        other -> crash (show other)
   in scope "given-goals" $
        tests
          [ scope "available-dictionary-does-not-determine-goal" $ required (unknown a) [a],
            scope "nested-inference-variables-are-rejected" $
              required (Type.arrow () (unknown a) (Type.app () (Type.list ()) (unknown b))) [a, b],
            scope "solved-goal-resolves" $
              case Goal.resolve [candidate] (ABT.substInheritAnnotation (Existential () a) (Type.nat ()) (unknown a)) of
                Right tree -> expectEqual (Given.resolvedGiven tree) (Given.Local dictionary)
                other -> crash (show other),
            scope "rigid-variable-does-not-become-concrete" $
              case Goal.resolve [candidate] (rigid a) of
                Left (Goal.ResolutionFailed Given.NoGiven {}) -> ok
                other -> crash (show other),
            scope "generic-dictionary-can-match-rigid-goal" $ do
              let generic = Given.Given (Given.Local dictionary) [a] [] (Type.app () (Type.list ()) (Type.var () a)) Given.Ambient
                  goal = Type.app () (Type.list ()) (rigid b)
              case Goal.resolve [generic] goal of
                Right tree -> expectEqual (Given.resolvedType tree) (TV.lowerType goal)
                other -> crash (show other),
            scope "existential-under-forall-is-not-hidden" $
              required (Type.forAll () (Universal b) (Type.arrow () (rigid b) (unknown a))) [a]
          ]
