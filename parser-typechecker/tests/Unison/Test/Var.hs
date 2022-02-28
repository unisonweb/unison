module Unison.Test.Var where

import EasyTest
import Unison.Symbol (Symbol)
import Unison.Var as Var

test :: Test ()
test =
  scope "var" $
    tests
      [ scope "free synthetic vars are universally quantifiable" $
          tests
            [ scope
                (Var.nameStr v)
                (expect $ Var.universallyQuantifyIfFree @Symbol v)
              | v <-
                  [ Var.inferAbility,
                    Var.inferInput,
                    Var.inferOutput,
                    Var.inferPatternPureE,
                    Var.inferPatternPureV,
                    Var.inferPatternBindE,
                    Var.inferPatternBindV,
                    Var.inferTypeConstructor,
                    Var.inferTypeConstructorArg
                  ]
            ]
      ]
