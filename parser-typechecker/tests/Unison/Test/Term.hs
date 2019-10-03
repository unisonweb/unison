{-# Language OverloadedStrings #-}

module Unison.Test.Term where

import EasyTest
import           Unison.Symbol ( Symbol )
import qualified Unison.Term   as Term
import qualified Unison.Type   as Type
import qualified Unison.Var    as Var

test :: Test ()
test = scope "term" $ tests [
  scope "Term.substTypeVar" $ do
    -- check that capture avoidance works in substTypeVar
    let v s = Var.nameds s :: Symbol
        tv s = Type.var() (v s)
        v1 s = Var.freshenId 1 (v s)
        tm :: Term.Term Symbol
        tm = Term.ann() (Term.ann()
                           (Term.nat() 42)
                           (Type.introOuter() (v "a") $
                             Type.arrow() (tv "a") (tv "x")))
                        (Type.forall() (v "a") (tv "a"))
        tm' = Term.substTypeVar (v "x") (tv "a") tm
        expected =
          Term.ann() (Term.ann()
                        (Term.nat() 42)
                        (Type.introOuter() (v1 "a") $
                          Type.arrow() (Type.var() $ v1 "a") (tv "a")))
                     (Type.forall() (v1 "a") (Type.var() $ v1 "a"))
    note $ show tm'
    note $ show expected
    expect $ tm == tm
    expect $ tm' == tm'
    expect $ tm' == expected
    ok
  ]
