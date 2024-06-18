{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Typechecker where

import EasyTest
import Unison.Symbol (Symbol (..))
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Var qualified as Var

test :: Test ()
test =
  scope "typechecker" $
    tests
      [ scope "isSubtype" isSubtypeTest
      ]

isSubtypeTest :: Test ()
isSubtypeTest =
  let symbol i n = Symbol i (Var.User n)
      forAll v t = Type.forall () v t
      var v = Type.var () v

      a = symbol 0 "a"
      a_ i = symbol i "a"
      lhs = forAll a (var a) -- ∀a. a
      rhs_ i = var (a_ i) -- a_i
   in -- check that `∀a. a <: a_i` (used to fail for i = 2, 3)
      tests [expectSubtype lhs (rhs_ i) | i <- [0 .. 5]]
  where
    expectSubtype t1 t2 =
      scope
        ("isSubtype (" <> show t1 <> ") (" <> show t2 <> ")")
        (expect $ Typechecker.isSubtype t1 t2)
