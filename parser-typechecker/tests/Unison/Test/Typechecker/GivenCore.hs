module Unison.Test.Typechecker.GivenCore (test) where

import Data.Foldable (toList)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import EasyTest
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Reference qualified as Reference
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker.Context qualified as Context
import Unison.Typechecker.GivenCore
import Unison.Typechecker.TypeLookup qualified as TL
import Unison.Var qualified as Var

test :: Test ()
test =
  scope "given-core" $
    let a = Var.named "a" :: Symbol
        x = Var.named "x"
        y = Var.named "y"
        d = Var.named "dict"
        n = Type.nat ()
        qualified = Type.implicitArrow () n n
        dictFunction = Reference.Builtin "Test.dictFunction"
        lookup = mempty {TL.typeOfTerms = Map.singleton dictFunction qualified}
        check bindings = verifyBindings PPE.empty [] lookup bindings
        apply argument = Term.app () (Term.ref () dictFunction) argument
        binding body typ = [(x, (), body, typ)]
     in tests
          [ scope "qualified-lambda" $
              expectAccepted (check (binding (Term.lam () ((), d) (Term.var () d)) qualified)),
            scope "explicit-dictionary-application" $
              expectAccepted (check (binding (apply (Term.nat () 42)) n)),
            scope "wrong-dictionary-type" $
              expectRejected (check (binding (apply (Term.text () "wrong")) n)),
            scope "missing-dictionary-argument" $
              expectRejected (check (binding (Term.ref () dictFunction) n)),
            scope "cannot-store-nat-as-polymorphic" $
              expectRejected (check (binding (Term.nat () 42) (Type.forAll () a (Type.var () a)))),
            scope "nested-annotations-are-lowered" $
              expectAccepted (check (binding (Term.ann () (Term.lam () ((), d) (Term.var () d)) qualified) qualified)),
            scope "components-reflect-inserted-dependencies" $
              let call v = Term.lam () ((), d) (Term.app () (Term.var () v) (Term.var () d))
               in case check [(x, (), call y, qualified), (y, (), call x, qualified)] of
                    Context.Success infos () ->
                      expect (any (\component -> List.sort [v | (v, _, _) <- component] == [x, y]) [component | Context.TopLevelComponent component <- toList infos])
                    result -> crash (describe result)
          ]
  where
    expectAccepted = \case
      Context.Success _ () -> ok
      result -> crash (describe result)
    expectRejected = \case
      Context.TypeError {} -> ok
      result -> crash ("expected a type error, got " <> describe result)
    describe = \case
      Context.Success _ () -> "success"
      Context.TypeError errors _ -> show errors
      Context.CompilerBug bug errors _ -> show (bug, errors)
