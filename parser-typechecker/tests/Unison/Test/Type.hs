{-# Language OverloadedStrings #-}

module Unison.Test.Type where

import EasyTest
import Unison.Type
import Unison.Symbol (Symbol)
import qualified Unison.Var as Var
import qualified Unison.Typechecker as Typechecker

infixr 1 -->

(-->) :: Ord v => Type v () -> Type v () -> Type v ()
(-->) a b = arrow() a b

test :: Test ()
test = scope "type" $ tests [
  scope "unArrows" $
    let x = arrow() (builtin() "a") (builtin() "b") :: Type Symbol ()
    in case x of
         Arrows' [i,o] ->
           expect (i == builtin() "a" && o == builtin() "b")
         _ -> crash "unArrows (a -> b) did not return a spine of [a,b]"
  ,
  scope "subtype" $ do
    let v = Var.named "a"
        v2 = Var.named "b"
        vt = var() v
        vt2 = var() v2
        x = forall() v (nat() --> effect() [vt, builtin() "eff"] (nat())) :: Type Symbol ()
        y = forall() v2 (nat() --> effect() [vt2] (nat())) :: Type Symbol ()
    expect . not $ Typechecker.isSubtype x y
  ]
