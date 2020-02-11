module Unison.Test.Util.TMap where

import EasyTest
import qualified Unison.Util.TrieMap as TMap


test :: Test ()
test = scope "Util.TMap" . tests $ [
    scope "empty ==" . expect $ (TMap.empty :: TMap.TMap () ()) == TMap.empty,
    scope "minUniquePrefix for empty list" . expect $ 0 == TMap.minUniquePrefix ((TMap.fromList []) :: TMap.TMap () ()),
    scope "minUniquePrefix for 1-item list" . expect $ 0 == TMap.minUniquePrefix (TMap.fromList [("test", ())]),
    scope "minUniquePrefix for 2-item list" . expect $ 2 == TMap.minUniquePrefix (TMap.fromList [("test", ()), ("tu", ())]),
    scope "minUniquePrefix for 3-item list" . expect $ 2 == TMap.minUniquePrefix (TMap.fromList [("test", ()), ("tu", ()), ("fest", ())]),
    scope "minUniquePrefix for 4-item list" . expect $ 5 == TMap.minUniquePrefix (TMap.fromList [("test", ()), ("tu", ()), ("fest", ()),  ("fester", ())])
  ]
