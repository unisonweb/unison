module Unison.Test.Typechecker.GivenPlan (test) where

import Data.List (permutations)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import EasyTest
import Unison.Blank qualified as Blank
import Unison.Parser.Ann (Ann (..), SiteId (..))
import Unison.Prelude
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.GivenApply qualified as Apply
import Unison.Typechecker.GivenGoal qualified as Goal
import Unison.Typechecker.GivenPlan
import Unison.Typechecker.GivenResolver qualified as Given
import Unison.Typechecker.TypeVar qualified as TV
import Unison.Var qualified as Var

test :: Test ()
test =
  let a = External
      site = SiteId 1
      nat = Type.nat a :: Type.Type Symbol Ann
      text = Type.text a
      ref = Reference.Builtin
      candidate name typ = Given.Given (Given.Global (ref name)) [] [] typ Given.Ambient
      ambient = [candidate "nat" nat, candidate "text" text]
      goal slot typ = Context.ConstraintGoal site slot a (TV.liftType typ) []
      parameter n = Var.freshenId n (Var.named "dictionary")
      intro n = Context.DictionaryParameter site a (parameter n)
      getPlan notes = either (crash . show) pure (plan ambient notes)
      binder = SiteId 3
      local = Var.named "forward"
      completed = Context.GivenBinding binder a local (Given.Lexical 1) (TV.liftType nat)
      scopes = Map.singleton site (Set.singleton binder)
      scoped = planWithVisibleGivens scopes ambient
   in scope "given-plan" $
        tests
          [ scope "note-order-does-not-change-argument-order" $
              for_ (permutations [goal 0 nat, goal 1 text, intro 9, intro 10]) \notes -> do
                result <- getPlan notes
                case Map.lookup site result of
                  Just (Apply.Rewrite parameters arguments) -> do
                    expectEqual parameters [parameter 9, parameter 10]
                    expectEqual arguments [Term.ref a (ref "nat"), Term.ref a (ref "text")]
                  Nothing -> crash "missing plan",
            scope "duplicate-argument-is-rejected" $
              case plan ambient [goal 0 nat, goal 0 nat] of
                Left (DuplicateArgument actual 0) -> expectEqual actual site
                _ -> crash "expected duplicate argument rejection",
            scope "missing-earlier-slot-is-rejected" $
              case plan ambient [goal 1 nat] of
                Left (MissingArgument actual) -> expectEqual actual site
                _ -> crash "expected missing argument rejection",
            scope "duplicate-parameter-is-rejected" $
              case plan ambient [intro 9, intro 9] of
                Left (DuplicateParameter actual _) -> expectEqual actual site
                _ -> crash "expected duplicate parameter rejection",
            scope "undetermined-goal-is-not-generalized" $
              let unknown = Type.var a (TV.Existential Blank.Blank (Var.named "unknown"))
               in case plan ambient [Context.ConstraintGoal site 0 a unknown []] of
                    Left (GoalFailure _ Goal.AnnotationRequired {}) -> ok
                    _ -> crash "expected goal annotation requirement",
            scope "undetermined-given-is-not-made-rigid" $
              let v = Var.named "given"
                  unknown = Type.var a (TV.Existential Blank.Blank (Var.named "unknown"))
               in case plan ambient [Context.ConstraintGoal site 0 a (TV.liftType nat) [(v, Given.Lexical 1, unknown)]] of
                    Left (GivenNeedsAnnotation _ actual) -> expectEqual actual v
                    _ -> crash "expected given annotation requirement",
            scope "forward-binding-note-order-independent" $
              for_ (permutations [goal 0 nat, completed]) \notes ->
                case scoped notes of
                  Right result -> case Map.lookup site result of
                    Just (Apply.Rewrite [] arguments) -> expectEqual arguments [Term.var a local]
                    _ -> crash "wrong forward plan"
                  Left err -> crash (show err),
            scope "completed-scheme-replaces-premature-snapshot" $ do
              let unknown = Type.var a (TV.Existential Blank.Blank (Var.named "unfinished"))
                  captured = Context.ConstraintGoal site 0 a (TV.liftType nat) [(local, Given.Lexical 1, unknown)]
              case scoped [captured, completed] of
                Right _ -> ok
                Left err -> crash (show err),
            scope "missing-completed-binding-rejected" $
              case scoped [goal 0 nat] of
                Left (MissingGivenBinding actual) -> expectEqual actual binder
                _ -> crash "expected missing binding rejection",
            scope "duplicate-completed-binding-rejected" $
              case scoped [goal 0 nat, completed, completed] of
                Left (DuplicateGivenBinding actual) -> expectEqual actual binder
                _ -> crash "expected duplicate binding rejection",
            scope "missing-source-scope-rejected" $
              case planWithVisibleGivens Map.empty ambient [goal 0 nat] of
                Left (MissingGoalScope actual) -> expectEqual actual site
                _ -> crash "expected missing scope rejection",
            scope "invisible-source-given-is-excluded" $ do
              let captured = Context.ConstraintGoal site 0 a (TV.liftType nat) [(local, Given.Lexical 9, TV.liftType nat)]
              case planWithVisibleGivens (Map.singleton site Set.empty) ambient [captured, completed] of
                Right result -> case Map.lookup site result of
                  Just (Apply.Rewrite [] arguments) -> expectEqual arguments [Term.ref a (ref "nat")]
                  _ -> crash "wrong scope plan"
                Left err -> crash (show err),
            scope "generated-parameters-retain-dynamic-scope" $ do
              let param = parameter 10
                  captured = Context.ConstraintGoal site 0 a (TV.liftType nat) [(param, Given.Parameter 1 10, TV.liftType nat)]
              case scoped [captured, completed] of
                Right result -> case Map.lookup site result of
                  Just (Apply.Rewrite [] arguments) -> expectEqual arguments [Term.var a param]
                  _ -> crash "wrong parameter plan"
                Left err -> crash (show err),
            scope "equal-locations-do-not-collapse-sites" $ do
              let second = Context.ConstraintGoal (SiteId 2) 0 a (TV.liftType text) []
              result <- getPlan [goal 0 nat, second]
              expectEqual (Map.size result) 2
          ]
