{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Typechecker.Context (test) where

import Data.Foldable (for_)
import EasyTest
import Unison.PrettyPrintEnv qualified as PPE
import Unison.Symbol (Symbol)
import Unison.Term qualified as Term
import Unison.Type qualified as Type
import Unison.Typechecker.Context qualified as Context
import Unison.Var qualified as Var

test :: Test ()
test =
  scope "context" $
    tests
      [ scope "verifyClosedTerm" verifyClosedTermTest
      ]

type TV = Context.TypeVar Symbol ()

verifyClosedTermTest :: Test ()
verifyClosedTermTest =
  tests
    [ scope "report-all-free-vars" $
        let a = Var.named @Symbol "a"
            b = Var.named @Symbol "b"
            a' = Var.named @TV "a'"
            b' = Var.named @TV "b'"
            -- (a : a')(b : b')
            t =
              Term.app
                ()
                (Term.ann () (Term.var () a) (Type.var () a'))
                (Term.ann () (Term.var () b) (Type.var () b'))
            res = Context.synthesizeClosed PPE.empty Context.PatternMatchCoverageCheckSwitch'Enabled [] mempty t
            errors = Context.typeErrors res
            expectUnknownSymbol (Context.ErrorNote cause _) = case cause of
              Context.UnknownSymbol _ _ -> ok
              e -> crash $ "Unexpected type error " <> show e
         in do
              expectEqual 4 (length errors) -- there are 4 unknown symbols: a, a', b, b'
              for_ errors expectUnknownSymbol
    ]
