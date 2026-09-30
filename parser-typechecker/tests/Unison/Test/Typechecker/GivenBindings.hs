module Unison.Test.Typechecker.GivenBindings (test) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import EasyTest
import Unison.ABT qualified as ABT
import Unison.Parser.Ann (Ann (..), siteId)
import Unison.Prelude
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.GivenSites
import Unison.Typechecker.TypeVar qualified as TV
import Unison.Var qualified as Var

test :: Test ()
test =
  let a = External
      f = Var.named "given" :: Symbol
      x = Var.named "argument"
      n = Type.nat a
      nodes node = node : foldMap nodes (ABT.out node)
      check top body =
        let prepared = prepare (Term.letRec' top [(f, a, body)] (Term.nat a 0))
            source = preparedTerm prepared
            marked =
              Map.fromList
                [ (site, (Map.findWithDefault v site (binderRenamings prepared), binderDepths prepared Map.! site))
                | node <- nodes source,
                  ABT.Abs v _ <- [ABT.out node],
                  v == f,
                  Just site <- [siteId (ABT.annotation node)]
                ]
         in Context.synthesizeClosedWithGivens siteId marked PPE.empty Context.PatternMatchCoverageCheckAndKindInferenceSwitch'Disabled Map.empty [] mempty (TV.liftTerm source)
      inspect action = \case
        Context.Success notes _ -> action [(site, v, typ) | Context.GivenBinding site _ v _ typ <- toList notes]
        Context.TypeError errors _ -> crash (show errors)
        Context.CompilerBug bug _ _ -> crash (show bug)
   in scope "given-binding-types" $
        tests
          [ scope "inferred-polymorphic-scheme-is-complete" $
              inspect
                ( \case
                    [(_, _, typ)] -> do
                      expect (Set.null (ABT.freeVars typ))
                      case typ of
                        Type.ForallNamed' _ _ -> ok
                        _ -> crash "expected generalized identity type"
                    _ -> crash "expected one binding note"
                )
                (check False (Term.lam a (a, x) (Term.var a x))),
            scope "recursive-probe-does-not-duplicate-binding-types" $
              let body = Term.ann a (Term.lam a (a, x) (Term.app a (Term.var a f) (Term.var a x))) (Type.arrow a n n)
               in inspect (\types -> expectEqual (length types) 1) (check True body),
            scope "nonrecursive-probe-does-not-duplicate-binding-types" $
              inspect
                ( \case
                    [(_, v, typ)] -> do
                      expectEqual v f
                      expectEqual (TV.lowerType typ) n
                    _ -> crash "expected one binding note"
                )
                (check True (Term.ann a (Term.nat a 42) n))
          ]
