{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import           EasyTest
import           System.Environment (getArgs)
import           System.IO
import qualified Unison.Test.DataDeclaration as DataDeclaration
import qualified Unison.Test.FileParser as FileParser
import qualified Unison.Test.Lexer as Lexer
import qualified Unison.Test.Range as Range
import qualified Unison.Test.TermParser as TermParser
import qualified Unison.Test.TermPrinter as TermPrinter
import qualified Unison.Test.Type as Type
import qualified Unison.Test.TypePrinter as TypePrinter
import qualified Unison.Test.Typechecker as Typechecker
import qualified Unison.Test.Typechecker.TypeError as TypeError
import qualified Unison.Test.ColorText as ColorText
import qualified Unison.Test.RemainingWork as RemainingWork

test :: Test ()
test = tests
  [ Lexer.test
  , TermParser.test
  , TermPrinter.test
  , Type.test
  , Typechecker.test
  , TypeError.test
  , TypePrinter.test
  , FileParser.test
  , DataDeclaration.test
  , Range.test
  , ColorText.test
  , RemainingWork.test
 ]

main :: IO ()
main = do
  args <- getArgs
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  runOnly (case args of [] -> ""; [prefix] -> prefix) test
