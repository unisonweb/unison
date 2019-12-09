{-# Language OverloadedStrings #-}

module Unison.Test.Typechecker where

import           EasyTest
import           Unison.Prelude
import           Unison.Symbol       ( Symbol(..) )
import           Unison.Type        ( Type )
import qualified Unison.Type        as Type
import qualified Unison.Typechecker as Typechecker
import qualified Unison.Var         as Var

infixr 1 -->

(-->) :: Ord v => Type v () -> Type v () -> Type v ()
(-->) a b = Type.arrow() a b

builtin :: Ord v => Text -> Type v ()
builtin = Type.builtin()

effect :: Ord v => [Type v ()] -> Type v () -> Type v ()
effect = Type.effect()

forall :: Ord v => v -> Type v () -> Type v ()
forall = Type.forall()

nat :: Ord v => Type v ()
nat = Type.nat()

var :: Symbol -> Type Symbol ()
var = Type.var()

test :: Test ()
test = scope "typechecker" $ tests
  [ scope "isSubtype" isSubtypeTest
  , scope "subtype" $ do
      let v = Var.named "a"
          v2 = Var.named "b"
          vt = var v
          vt2 = var v2
          x = forall v (nat --> effect [vt, builtin "eff"] nat) :: Type Symbol ()
          y = forall v2 (nat --> effect [vt2] nat) :: Type Symbol ()
      expect . not $ Typechecker.isSubtype x y
  ]

isSubtypeTest :: Test ()
isSubtypeTest =
  let
    symbol i n = Symbol i (Var.User n)

    a = symbol 0 "a"
    a_ i = symbol i "a"
    lhs = forall a (var a) -- ∀a. a
    rhs_ i = var (a_ i)    -- a_i
  in
    -- check that `∀a. a <: a_i` (used to fail for i = 2, 3)
    tests [ expectSubtype lhs (rhs_ i) | i <- [0 .. 5] ]
  where
    expectSubtype t1 t2 =
     scope ("isSubtype (" <> show t1 <> ") (" <> show t2 <> ")")
           (expect $ Typechecker.isSubtype t1 t2)
