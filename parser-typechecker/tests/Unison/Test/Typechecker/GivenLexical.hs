module Unison.Test.Typechecker.GivenLexical (test) where

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
import Unison.Typechecker.GivenResolver qualified as Given
import Unison.Typechecker.GivenSites
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Typechecker.TypeVar qualified as TV
import Unison.Var qualified as Var

test :: Test ()
test =
  let a = External
      n = Type.nat a :: Type.Type Symbol Ann
      x = Var.named "ordinary"
      fun = Reference.Builtin "qualified"
      reference = Term.ref a fun
      block value body = Term.letRec' False [(x, a, Term.nat a value)] body
      nodes node = node : foldMap nodes (ABT.out node)
      run selected original =
        let p = prepare original
            numbered = preparedTerm p
            binderSites = [site | node <- nodes numbered, ABT.Abs v _ <- [ABT.out node], v == x, Just site <- [siteId (ABT.annotation node)]]
            marked = Map.fromList [(site, (binderRenamings p Map.! site, binderDepths p Map.! site)) | site <- selected binderSites]
            lookup = TL.TypeLookup (Map.singleton fun (Type.implicitArrow a n n)) Map.empty Map.empty
            result = Context.synthesizeClosedWithGivens siteId marked PPE.empty Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled Map.empty [] lookup (TV.liftTerm numbered)
         in (marked, result)
      scopes notes = [givens | Context.ConstraintGoal _ _ _ _ givens <- toList notes]
      success action = \case
        Context.Success notes _ -> action (scopes notes)
        Context.TypeError errors _ -> crash (show errors)
        Context.CompilerBug bug _ _ -> crash (show bug)
   in scope "lexical-given-checker" $
        tests
          [ scope "nested-bindings-have-distinct-identities-and-depths" $ do
              let (marked, result) = run id (block 1 (block 2 reference))
              success
                ( \case
                    [givens] -> do
                      expectEqual (Set.fromList [(v, d) | (v, d, _) <- givens]) (Set.fromList (Map.elems marked))
                      expectEqual (Set.fromList [d | (_, d, _) <- givens]) (Set.fromList [1, 2])
                      expect (all (\(_, _, t) -> TV.lowerType t == n) givens)
                      let pool = [Given.givenFromType (Given.Local v) (Given.Lexical d) (TV.lowerType t) | (v, d, t) <- givens]
                      case Given.resolve pool n of
                        Right tree -> expectEqual (Given.resolvedGiven tree) (Given.Local (fst (last (Map.elems marked))))
                        other -> crash (show other)
                    _ -> crash "expected one goal"
                )
                result,
            scope "unmarked-same-name-binding-does-not-become-given" $ do
              let (marked, result) = run (take 1) (block 1 (block 2 reference))
              success
                ( \case
                    [givens] -> expectEqual [(v, d) | (v, d, _) <- givens] (Map.elems marked)
                    _ -> crash "expected one goal"
                )
                result,
            scope "marks-do-not-leak-between-sibling-scopes" $ do
              let source = Term.list a [block 1 (Term.nat a 0), block 2 reference]
                  (_, result) = run (take 1) source
              success
                ( \case
                    [givens] -> expect (null givens)
                    _ -> crash "expected one goal"
                )
                result
          ]
