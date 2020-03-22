{-# Language OverloadedStrings #-}

module Unison.Test.ABT where

import Data.Set as Set
import EasyTest
import Unison.ABT as ABT
import Unison.Symbol (Symbol(..))
import Unison.Var as Var

test :: Test ()
test = scope "abt" $ tests [
  scope "freshInBoth" $
    let
      symbol i n = Symbol i (Var.User n)
      var i n = ABT.var $ symbol i n
      t1 = var 1 "a"
      t2 = var 0 "a"
      fresh = ABT.freshInBoth t1 t2 $ symbol 0 "a"
    in tests
      [ scope "first"  $ expect (not $ Set.member fresh (ABT.freeVars t1))
      , scope "second" $ expect (not $ Set.member fresh (ABT.freeVars t2))
      ]
  ]
