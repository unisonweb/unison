module Unison.Test.Typechecker.GivenGoals (test) where

import Data.Map.Strict qualified as Map
import EasyTest
import Unison.ABT qualified as ABT
import Unison.Parser.Ann (Ann (..), siteId)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.GivenGoal qualified as Goal
import Unison.Typechecker.GivenSites
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Typechecker.TypeVar qualified as TV
import Unison.Var qualified as Var

test :: Test ()
test =
  let a = External
      n = Type.nat a :: Type.Type Symbol Ann
      qualified = Type.implicitArrow a n n
      fun = Reference.Builtin "qualified"
      reference = Term.ref a fun
      check typ term =
        let numbered = preparedTerm (prepare term)
            lookup = TL.TypeLookup (Map.singleton fun typ) Map.empty Map.empty
            result = Context.synthesizeClosedWithImplicits siteId PPE.empty Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled Map.empty [] lookup (TV.liftTerm numbered)
         in (numbered, result)
      goals notes = [(site, slot, ty) | Context.ConstraintGoal site slot _ ty _ <- toList notes]
   in scope "given-goal-checker" $
        tests
          [ scope "goal-at-reference-site" $
              case check qualified reference of
                (numbered, Context.Success notes typ) -> do
                  expectEqual (TV.lowerType typ) n
                  case goals notes of
                    [(site, 0, goal)] -> do
                      expectEqual (Just site) (siteId (ABT.annotation numbered))
                      expectEqual (TV.lowerType goal) n
                    _ -> crash "expected one dictionary goal"
                (_, Context.TypeError es _) -> crash (show es)
                (_, Context.CompilerBug bug _ _) -> crash (show bug),
            scope "interior-arrow-has-prefix-site" $ do
              let typ = Type.arrow a n (Type.implicitArrow a n (Type.arrow a n n))
                  term = Term.app a (Term.app a reference (Term.nat a 1)) (Term.nat a 2)
              case check typ term of
                (Term.App' prefix _, Context.Success notes _) ->
                  case goals notes of
                    [(site, 0, _)] -> expectEqual (Just site) (siteId (ABT.annotation prefix))
                    _ -> crash (show notes)
                (_, Context.TypeError es _) -> crash (show es)
                (_, Context.CompilerBug bug _ _) -> crash (show bug)
                _ -> crash "wrong application shape",
            scope "unsolved-goals-survive-generalization" $ do
              let variable = Var.named "a"
                  identity = Type.forAll a variable (Type.implicitArrow a (Type.var a variable) (Type.var a variable))
              case check identity reference of
                (_, Context.Success notes _) -> case goals notes of
                  [(_, _, goal)] -> case Goal.resolve [] goal of
                    Left Goal.AnnotationRequired {} -> ok
                    other -> crash (show other)
                  _ -> crash (show notes)
                (_, Context.TypeError es _) -> crash (show es)
                (_, Context.CompilerBug bug _ _) -> crash (show bug),
            scope "ordinary-inference-determines-goals" $ do
              let variable = Var.named "a"
                  identity = Type.forAll a variable (Type.implicitArrow a (Type.var a variable) (Type.var a variable))
              case check identity (Term.ann a reference n) of
                (_, Context.Success notes _) -> case goals notes of
                  [(_, _, goal)] -> expectEqual (TV.lowerType goal) n
                  _ -> crash (show notes)
                (_, Context.TypeError es _) -> crash (show es)
                (_, Context.CompilerBug bug _ _) -> crash (show bug),
            scope "annotation-probe-does-not-duplicate-goals" $ do
              let variable = Var.named "binding"
                  file = Term.letRec' True [(variable, a, Term.ann a reference n)] (Term.var a variable)
              case check qualified file of
                (_, Context.Success notes _) -> expectEqual (length (goals notes)) 1
                (_, Context.TypeError es _) -> crash (show es)
                (_, Context.CompilerBug bug _ _) -> crash (show bug),
            scope "consecutive-goals-retain-argument-order" $
              case check (Type.implicitArrow a n (Type.implicitArrow a (Type.text a) n)) reference of
                (_, Context.Success notes _) ->
                  expectEqual [(slot, TV.lowerType typ) | (_, slot, typ) <- goals notes] [(0, n), (1, Type.text a)]
                (_, Context.TypeError es _) -> crash (show es)
                (_, Context.CompilerBug bug _ _) -> crash (show bug)
          ]
