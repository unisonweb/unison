module Main where

import Control.Monad
import EasyTest

suite1 :: Test ()
suite1 =
  tests
    [ scope "a" ok,
      scope "b.c" ok,
      scope "b" ok,
      -- leaving this commented out until we have a function like:
      -- `expectFailure :: Scope -> Test a -> Test a`
      -- , scope "b" . scope "c" $ error "oh noes! - should fail with b.c scope"
      scope "b" . scope "c" . scope "d" $ ok,
      scope "c" ok
    ]

suite2 :: Test ()
suite2 =
  tests
    [ scope "pending.failure" (pending (expectEqual True False))
    -- , scope "pending.success" (pending ok)
    ]

reverseTest :: Test ()
reverseTest = scope "list reversal" $ do
  nums <- listsOf [0 .. 100] (int' 0 99)
  nums `forM_` \nums -> expect (reverse (reverse nums) == nums)

main :: IO ()
main = do
  run suite1
  runOnly "a" suite1
  runOnly "b" suite1
  runOnly "b" $ tests [suite1, scope "xyz" (crash "never run")]
  runOnly "b.c" $ tests [suite1, scope "b" (crash "never run")]
  run reverseTest
  run suite2
