{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Typechecker where

import EasyTest
import Unison.Result (pattern Result)
import Unison.Symbol (Symbol (..))
import Unison.Test.Common qualified as Common
import Unison.Type qualified as Type
import Unison.Typechecker qualified as Typechecker
import Unison.Var qualified as Var

test :: Test ()
test =
  scope "typechecker" $
    tests
      [ scope "isSubtype" isSubtypeTest,
        scope "type-directed resolution batches repeated overloaded operators" $
          case Common.parseAndSynthesizeAsFile [] "> tdnr-batch"
            (unlines ["f : Int -> Int", "f x = x + 1 + 2 + 3"]) of
            Result _ (Just _) -> ok
            Result notes _ -> crash ("TDNR failed to resolve a repeated overloaded operator: " ++ show notes)
      ]

isSubtypeTest :: Test ()
isSubtypeTest =
  let symbol i n = Symbol i (Var.User n)
      forAll v t = Type.forAll () v t
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
