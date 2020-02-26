{-# Language OverloadedStrings #-}
{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}

module Unison.Test.ABT where

import Prelude hiding (abs)
import Prelude.Extras (Eq1(..))
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
  , scope "capture" capture
  ]

data P a = Z | T a a deriving (Functor, Foldable, Traversable, Eq, Ord)

instance Eq1 P where
  x ==# y = x == y

capture :: Test ()
capture = expect (subst u (var x0) tm1 == subst u (var x0) tm2)
 where
 tm1 = abs x0 $ abs x1 $ tm $ T (var x0) (var u)
 tm2 = abs x0 $ abs x2 $ tm $ T (var x0) (var u)

 u  = Symbol 0 (Var.User "u")
 x0 = Symbol 0 (Var.User "x")
 x1 = Symbol 1 (Var.User "x")
 x2 = Symbol 2 (Var.User "x")
