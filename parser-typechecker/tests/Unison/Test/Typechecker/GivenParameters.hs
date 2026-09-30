module Unison.Test.Typechecker.GivenParameters (test) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import EasyTest
import Unison.Parser.Ann (Ann (..), siteId)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.GivenApply qualified as Apply
import Unison.Typechecker.GivenCore qualified as Core
import Unison.Typechecker.GivenGoal qualified as Goal
import Unison.Typechecker.GivenResolver qualified as Given
import Unison.Typechecker.GivenSites
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Typechecker.TypeVar qualified as TV
import Unison.Var qualified as Var

test :: Test ()
test =
  let a = External
      n = Type.nat a :: Type.Type Symbol Ann
      f = Var.named "f"
      x = Var.named "dictionary"
      summon = Reference.Builtin "summon"
      tv = Var.named "a"
      identity = Type.forAll a tv (Type.implicitArrow a (Type.var a tv) (Type.var a tv))
      reference = Term.ref a summon
      check = checkMarked (const Map.empty)
      checkMarked select typ body =
        let source = Term.letRec' True [(f, a, Term.ann a body typ)] (Term.nat a 0)
            p = prepare source
            config = Context.ParameterConfig (expressionDepths p) (Set.fromList (Map.elems (binderRenamings p)))
            lookup = TL.TypeLookup (Map.singleton summon identity) Map.empty Map.empty
         in (p, lookup, Context.synthesizeClosedWithParameters siteId (select p) config PPE.empty Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled Map.empty [] lookup (TV.liftTerm (preparedTerm p)))
      inspect action (prepared, lookup, result) = case result of
        Context.Success notes _ -> do
          action (toList notes)
          expect (Set.disjoint (Set.fromList (parameters (toList notes))) (Set.fromList (Map.elems (binderRenamings prepared))))
          let merge (Apply.Rewrite ps as) (Apply.Rewrite qs bs) = Apply.Rewrite (qs <> ps) (bs <> as)
              add edits note = case note of
                Context.DictionaryParameter site _ v -> pure (Map.insertWith merge site (Apply.Rewrite [v] []) edits)
                Context.ConstraintGoal site _ location goal givens ->
                  let pool = [Given.givenFromType (Given.Local v) scope (TV.lowerType typ) | (v, scope, typ) <- givens]
                   in case Goal.resolve pool goal of
                        Right tree -> pure (Map.insertWith merge site (Apply.Rewrite [] [Apply.dictionaryTerm location tree]) edits)
                        Left err -> crash (show err)
                _ -> pure edits
          edits <- foldM add Map.empty notes
          rewritten <- either (crash . show) pure (Apply.apply edits (renameLocals prepared))
          case rewritten of
            Term.LetRecNamedAnnotatedTop' True _ bindings _ -> do
              let types = Map.fromList [(v, t) | Context.TopLevelComponent cs <- toList notes, (v, t, _) <- cs]
                  checked = [(v, loc, body, types Map.! v) | ((loc, v), body) <- bindings]
              case Core.verifyBindings PPE.empty [] lookup checked of
                Context.Success _ () -> ok
                Context.TypeError errors _ -> crash (show errors)
                Context.CompilerBug bug _ _ -> crash (show bug)
            _ -> crash "unexpected rewritten file"
        Context.TypeError errors _ -> crash (show errors)
        Context.CompilerBug bug _ _ -> crash (show bug)
      parameters notes = [v | Context.DictionaryParameter _ _ v <- notes]
      goals notes = [(goal, givens) | Context.ConstraintGoal _ _ _ goal givens <- notes]
      choices notes = for (goals notes) \(goal, givens) ->
        let pool = [Given.givenFromType (Given.Local v) scope (TV.lowerType typ) | (v, scope, typ) <- givens]
         in case Goal.resolve pool goal of
              Right tree -> pure (Given.resolvedGiven tree)
              Left err -> crash (show err)
   in scope "given-parameters" $
        tests
          [ scope "qualified-definition-keeps-its-signature" $
              inspect
                ( \notes -> do
                    expectEqual (length (parameters notes)) 1
                    expectEqual (length (goals notes)) 1
                    case [typ | Context.TopLevelComponent bindings <- notes, (v, typ, _) <- bindings, v == f] of
                      [typ@Type.ImplicitArrow' {}] ->
                        case Context.isEqual (TV.liftType (void typ)) (TV.liftType (void (Type.implicitArrow a n n))) of
                          Right equal -> expect equal
                          Left bug -> crash (show bug)
                      _ -> crash "qualified signature was not retained"
                    selected <- choices notes
                    expectEqual selected (map Given.Local (parameters notes))
                )
                (check (Type.implicitArrow a n n) reference),
            scope "rigid-polymorphic-parameter" $
              inspect
                ( \notes -> do
                    expectEqual (length (parameters notes)) 1
                    selected <- choices notes
                    expectEqual selected (map Given.Local (parameters notes))
                )
                (check identity reference),
            scope "does-not-consume-user-lambda-as-dictionary" $
              inspect
                ( \notes -> do
                    expectEqual (length (parameters notes)) 1
                    expectEqual (length (goals notes)) 0
                )
                (check (Type.implicitArrow a n (Type.arrow a n n)) (Term.lam a (a, x) (Term.var a x))),
            scope "same-named-explicit-argument-does-not-capture-dictionary" $
              inspect
                ( \notes -> do
                    selected <- choices notes
                    expectEqual selected (map Given.Local (parameters notes))
                )
                (check (Type.implicitArrow a n (Type.arrow a n n)) (Term.lam a (a, x) reference)),
            scope "local-given-shadows-generated-parameter" $ do
              let select prepared = Map.intersectionWith (,) (binderRenamings prepared) (binderDepths prepared)
                  body = Term.letRec' False [(x, a, Term.nat a 7)] reference
              inspect
                ( \notes -> do
                    selected <- choices notes
                    expectEqual (length selected) 1
                    expect (all (`notElem` map Given.Local (parameters notes)) selected)
                )
                (checkMarked select (Type.implicitArrow a n n) body),
            scope "later-implicit-parameter-shadows-earlier" $
              inspect
                ( \notes -> do
                    expectEqual (length (parameters notes)) 2
                    selected <- choices notes
                    expectEqual selected [Given.Local (last (parameters notes))]
                )
                (check (Type.implicitArrow a n (Type.implicitArrow a n n)) reference)
          ]
