{- ORMOLU_DISABLE -} -- Remove this when the file is ready to be auto-formatted
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
      t1 = var 1 "a"
      t2 = var 0 "a"
      fresh = ABT.freshInBoth t1 t2 $ symbol 0 "a"
    in tests
      [ scope "first"  $ expect (not $ Set.member fresh (ABT.freeVars t1))
      , scope "second" $ expect (not $ Set.member fresh (ABT.freeVars t2))
      ],
  scope "rename" $ do
    -- rename x to a in \a  -> [a, x] should yield
    --                  \a1 -> [a1, a]
    let t1 = ABT.abs (symbol 0 "a") (ABT.tm [var 0 "a", var 0 "x"])
        t2 = ABT.rename (symbol 0 "x") (symbol 0 "a") t1
        fvs = toList . ABT.freeVars $ t2
    -- make sure the variable wasn't captured
    expectEqual fvs [symbol 0 "a"]
    -- make sure the resulting term is alpha equiv to \a1 -> [a1, a]
    expectEqual t2 (ABT.abs (symbol 0 "b") (ABT.tm [var 0 "b", var 0 "a"]))
  ]
  where
    symbol i n = Symbol i (Var.User n)
    var i n = ABT.var $ symbol i n
