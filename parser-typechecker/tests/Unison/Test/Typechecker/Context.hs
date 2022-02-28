{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Typechecker.Context (test) where

import Data.Foldable (for_)
import EasyTest
import Unison.Symbol (Symbol)
import qualified Unison.Term as Term
import qualified Unison.Type as Type
import qualified Unison.Typechecker.Context as Context
import qualified Unison.Var as Var

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
            res = Context.synthesizeClosed [] mempty t
            errors = Context.typeErrors res
            expectUnknownSymbol (Context.ErrorNote cause _) = case cause of
              Context.UnknownSymbol _ _ -> ok
              e -> crash $ "Unexpected type error " <> show e
         in do
              expectEqual 4 (length errors) -- there are 4 unknown symbols: a, a', b, b'
              for_ errors expectUnknownSymbol
    ]
