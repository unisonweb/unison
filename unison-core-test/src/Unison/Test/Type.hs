{-# Language OverloadedStrings #-}

module Unison.Test.Type where

import EasyTest
import Unison.Type
import Unison.Symbol (Symbol)

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
  ]
