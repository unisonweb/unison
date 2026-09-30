module Unison.Test.Typechecker.GivenFinish (test) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
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
import Unison.Typechecker.GivenFinish
import Unison.Typechecker.GivenGoal qualified as Goal
import Unison.Typechecker.GivenPlan qualified as Plan
import Unison.Typechecker.GivenSites qualified as Sites
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Typechecker.TypeVar qualified as TV
import Unison.Var qualified as Var

test :: Test ()
test =
  let a = External
      f = Var.named "f" :: Symbol
      d = Var.named "dictionary"
      ident = Var.named "identity"
      x = Var.named "x"
      b = Var.named "b"
      n = Type.nat a
      summon = Reference.Builtin "summon"
      lookup = TL.TypeLookup (Map.singleton summon (Type.forAll a b (Type.implicitArrow a (Type.var a b) (Type.var a b)))) Map.empty Map.empty
      request = Term.ref a summon
      annotatedRequest = Term.ann a request n
      nodes node = node : foldMap nodes (ABT.out node)
      setup bindings =
        let prepared = Sites.prepare (Term.letRec' True bindings (Term.nat a 0))
            marked = Map.fromList [(site, (v, Sites.binderDepths prepared Map.! site)) | node <- nodes (Sites.preparedTerm prepared), ABT.Abs v _ <- [ABT.out node], v == d, Just site <- [siteId (ABT.annotation node)]]
            config = Context.ParameterConfig (Sites.expressionDepths prepared) (Set.fromList (Map.elems (Sites.binderRenamings prepared)))
            result = Context.synthesizeClosedWithParameters siteId marked config PPE.empty Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled Map.empty [] lookup (TV.liftTerm (Sites.preparedTerm prepared))
         in (prepared, Map.keysSet marked, result)
      inspect bindings action = case setup bindings of
        (prepared, marked, Context.Success notes _) -> action prepared marked (toList notes)
        (_, _, Context.TypeError errors _) -> crash (show errors)
        (_, _, Context.CompilerBug bug _ _) -> crash (show bug)
      complete prepared marked notes = finish PPE.empty [] lookup prepared marked [] notes
      accepted prepared marked notes = either (crash . show) pure (complete prepared marked notes)
      simple = [(f, a, annotatedRequest), (d, a, Term.nat a 42)]
   in scope "given-finish" $
        tests
          [ scope "forward-given-inserted-and-core-checked" $
              inspect simple \prepared marked notes -> do
                result <- accepted prepared marked notes
                expectEqual (Set.fromList [v | group <- components result, (v, _, _) <- group]) (Set.fromList [f, d])
                case [body | group <- components result, (v, body, _) <- group, v == f] of
                  [Term.Ann' (Term.App' (Term.Ref' ref) (Term.Var' choice)) _] -> do
                    expectEqual ref summon
                    expectEqual choice d
                  _ -> crash "dictionary was not inserted into forward reference",
            scope "insertion-recomputes-recursive-components" $
              inspect [(f, a, Term.lam a (a, x) (Term.app a (Term.ann a request (Type.arrow a n n)) (Term.var a x))), (d, a, Term.var a f)] \prepared marked notes -> do
                expect (all ((== 1) . length) [cs | Context.TopLevelComponent cs <- notes])
                result <- accepted prepared marked notes
                expectEqual (map (Set.fromList . map (\(v, _, _) -> v)) (components result)) [Set.fromList [f, d]],
            scope "insertion-cannot-introduce-unguarded-cycle" $
              inspect [(f, a, annotatedRequest), (d, a, Term.var a f)] \prepared marked notes ->
                case complete prepared marked notes of
                  Left CoreTypeErrors {} -> ok
                  Left err -> crash (show err)
                  Right _ -> crash "unguarded dictionary dependency cycle accepted",
            scope "ordinary-let-polymorphism-survives" $
              inspect
                ( simple
                    <> [ (ident, a, Term.lam a (a, x) (Term.var a x)),
                         (Var.named "number", a, Term.app a (Term.var a ident) (Term.nat a 1)),
                         (Var.named "text", a, Term.app a (Term.var a ident) (Term.text a "hello"))
                       ]
                )
                \prepared marked notes -> do
                  result <- accepted prepared marked notes
                  expectEqual (sum (map length (components result))) 5,
            scope "undetermined-summon-rejected-before-storage" $
              inspect [(f, a, request), (d, a, Term.nat a 42)] \prepared marked notes ->
                case complete prepared marked notes of
                  Left (PlanFailure (Plan.GoalFailure _ Goal.AnnotationRequired {})) -> ok
                  Left err -> crash (show err)
                  Right _ -> crash "undetermined summon was accepted",
            scope "false-polymorphic-signature-fails-core-check" $
              inspect [(f, a, Term.nat a 42)] \prepared marked _ ->
                case complete prepared marked [Context.TopLevelComponent [(f, Type.forAll a b (Type.var a b), False)]] of
                  Left CoreTypeErrors {} -> ok
                  Left err -> crash (show err)
                  Right _ -> crash "Nat body accepted at forall a. a",
            scope "qualified-signature-retained-after-lowering" $
              inspect [(f, a, Term.ann a request (Type.implicitArrow a n n))] \prepared marked notes -> do
                result <- accepted prepared marked notes
                case components result of
                  [[(_, _, Type.ImplicitArrow' _ _)]] -> ok
                  _ -> crash "qualified signature was lost",
            scope "missing-signature-rejected" $
              inspect simple \prepared marked notes ->
                case complete prepared marked [note | note <- notes, case note of Context.TopLevelComponent {} -> False; _ -> True] of
                  Left SignatureMismatch -> ok
                  Left err -> crash (show err)
                  Right _ -> crash "missing signature accepted",
            scope "duplicate-signature-rejected" $
              inspect simple \prepared marked notes ->
                case complete prepared marked (notes <> [Context.TopLevelComponent [(f, n, False)]]) of
                  Left (DuplicateSignature actual) -> expectEqual actual f
                  Left err -> crash (show err)
                  Right _ -> crash "duplicate signature accepted"
          ]
