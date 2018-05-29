module Main where

import EasyTest
import System.IO
import qualified Unison.Test.Common as Common
import qualified Unison.Test.TermParser as TermParser
import qualified Unison.Test.FileParser as FileParser

test :: Test ()
test = scope "unison" $ tests [
  TermParser.test
  ,
  FileParser.test
 ]

main :: IO ()
main = do
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  run test
