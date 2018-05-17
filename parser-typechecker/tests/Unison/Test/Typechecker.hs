module Unison.Test.Typechecker where

import EasyTest
import Unison.Test.Common

test = scope "typechecker" . tests . map expect $
  [
    c "x -> x"
      "forall a . a -> a"

  , c "x y -> x"
      "forall a b . a -> b -> a"

  , c "(+_Int64)"
      "Int64 -> Int64 -> Int64"
  ]
  where c = check
