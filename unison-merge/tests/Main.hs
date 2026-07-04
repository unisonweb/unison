module Main (main) where

import EasyTest
import System.IO.CodePage (withCP65001)
import Unison.Test.Merge.GivenSet qualified as GivenSet

main :: IO ()
main =
  withCP65001 (run (scope "unison-merge" tests))
  where
    tests :: Test ()
    tests =
      EasyTest.tests
        [ GivenSet.test
        ]
