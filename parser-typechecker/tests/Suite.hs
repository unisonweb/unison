module Main where

import EasyTest
import System.IO
import qualified Unison.Test.Common as Common
import qualified Unison.Test.Typechecker as Typechecker
import qualified Unison.Test.Typechecker.Components as Components

test :: Test ()
test = scope "unison" $ tests [ Typechecker.test, Components.test ]

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  run test
