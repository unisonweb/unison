{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import EasyTest
import System.Environment (getArgs)
import System.IO
import qualified Unison.Core.Test.Name as Name
import qualified Unison.Test.ABT as ABT
import qualified Unison.Test.ANF as ANF
import qualified Unison.Test.Cache as Cache
import qualified Unison.Test.Codebase as Codebase
import qualified Unison.Test.Codebase.Causal as Causal
import qualified Unison.Test.Codebase.FileCodebase as FileCodebase
import qualified Unison.Test.Codebase.Path as Path
import qualified Unison.Test.ColorText as ColorText
import qualified Unison.Test.DataDeclaration as DataDeclaration
import qualified Unison.Test.FileParser as FileParser
import qualified Unison.Test.Git as Git
import qualified Unison.Test.IO as TestIO
import qualified Unison.Test.Lexer as Lexer
import qualified Unison.Test.MCode as MCode
import qualified Unison.Test.Range as Range
import qualified Unison.Test.Referent as Referent
import qualified Unison.Test.Term as Term
import qualified Unison.Test.TermParser as TermParser
import qualified Unison.Test.TermPrinter as TermPrinter
import qualified Unison.Test.Type as Type
import qualified Unison.Test.TypePrinter as TypePrinter
import qualified Unison.Test.Typechecker as Typechecker
import qualified Unison.Test.Typechecker.Context as Context
import qualified Unison.Test.Typechecker.TypeError as TypeError
import qualified Unison.Test.UnisonSources as UnisonSources
import qualified Unison.Test.UriParser as UriParser
import qualified Unison.Test.Util.Bytes as Bytes
import qualified Unison.Test.Util.PinBoard as PinBoard
import qualified Unison.Test.Util.Pretty as Pretty
import qualified Unison.Test.Var as Var
import qualified Unison.Test.VersionParser as VersionParser

test :: Bool -> Test ()
test rt =
  tests
    [ Cache.test,
      Lexer.test,
      Term.test,
      TermParser.test,
      TermPrinter.test,
      Type.test,
      TypeError.test,
      TypePrinter.test,
      UnisonSources.test rt,
      FileParser.test,
      DataDeclaration.test,
      Range.test,
      ColorText.test,
      Bytes.test,
      Path.test,
      Causal.test,
      Referent.test,
      FileCodebase.test,
      ABT.test,
      ANF.test,
      MCode.test,
      Var.test,
      Codebase.test,
      Typechecker.test,
      UriParser.test,
      Context.test,
      Git.test,
      TestIO.test,
      Name.test,
      VersionParser.test,
      Pretty.test,
      PinBoard.test
    ]

main :: IO ()
main = do
  args0 <- getArgs
  let (rt, args)
        | "--new-runtime" : rest <- args0 = (True, rest)
        | otherwise = (False, args0)
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  case args of
    [] -> runOnly "" (test rt)
    [prefix] -> runOnly prefix (test rt)
    [seed, prefix] -> rerunOnly (read seed) prefix (test rt)
