module Unison.Test.Typechecker where

import EasyTest
import Unison.Test.Common

test = scope "typechecker" . tests $
  [
    c "x -> x"
      "forall a . a -> a"

  , c "x y -> x"
      "forall a b . a -> b -> a"

  , c "(+_Int64)"
      "Int64 -> Int64 -> Int64"

  , c "3"
      "UInt64"

  , c "+3"
      "Int64"

  , c "3.0"
      "Float"

  , c "Boolean.not true"
      "Boolean"

  , c "Boolean.not"
      "Boolean -> Boolean"

  , c "\"Hello, world!\""
      "Text"
  ]
  where c tm typ = scope tm $ expect $ check tm typ
