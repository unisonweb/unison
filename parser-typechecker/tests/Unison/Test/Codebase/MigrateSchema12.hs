module Unison.Test.Codebase.MigrateSchema12 where

{-
testType :: Type v a
testType = _

test :: Test ()
test =
  scope "migrate12"
    . tests
    $ [ scope "threeWayMerge.ex1"
        .  expect $ Causal.head testThreeWay == Set.fromList [3, 4]
      ]
-}