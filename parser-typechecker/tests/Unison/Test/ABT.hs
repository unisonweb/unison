{-# Language OverloadedStrings #-}

module Unison.Test.ABT where

import Data.Set as Set
import EasyTest
import Unison.ABT as ABT
import Unison.Symbol (Symbol(..))
import Unison.Var as Var
import           Unison.Codebase.Serialization    ( getFromBytes, putBytes )
import qualified Unison.Codebase.Serialization.V1 as V1

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
      ],
  -- confirmation of fix for https://github.com/unisonweb/unison/issues/1388
  -- where symbols with nonzero freshIds did not round trip
  scope "putSymbol" $ let
    v = Symbol 10 (User "hi")
    v' = getFromBytes V1.getSymbol (putBytes V1.putSymbol v)
    in expectEqual (Just v) v'
  ]
