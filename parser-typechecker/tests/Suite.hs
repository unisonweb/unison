{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import EasyTest
import System.Environment (getArgs)
import System.IO
import System.IO.CodePage (withCP65001)
import qualified Unison.Core.Test.Name as Name
import qualified Unison.Test.ABT as ABT
import qualified Unison.Test.ANF as ANF
import qualified Unison.Test.Codebase.Branch as Branch
import qualified Unison.Test.Codebase.Causal as Causal
import qualified Unison.Test.Codebase.Path as Path
import qualified Unison.Test.CodebaseInit as CodebaseInit
import qualified Unison.Test.DataDeclaration as DataDeclaration
import qualified Unison.Test.MCode as MCode
import qualified Unison.Test.Referent as Referent
import qualified Unison.Test.Syntax.FileParser as FileParser
import qualified Unison.Test.Syntax.TermParser as TermParser
import qualified Unison.Test.Syntax.TermPrinter as TermPrinter
import qualified Unison.Test.Syntax.TypePrinter as TypePrinter
import qualified Unison.Test.Term as Term
import qualified Unison.Test.Type as Type
import qualified Unison.Test.Typechecker as Typechecker
import qualified Unison.Test.Typechecker.Context as Context
import qualified Unison.Test.Typechecker.TypeError as TypeError
import qualified Unison.Test.UnisonSources as UnisonSources
import qualified Unison.Test.Util.PinBoard as PinBoard
import qualified Unison.Test.Util.Relation as Relation
import qualified Unison.Test.Util.Text as Text
import qualified Unison.Test.Var as Var

test :: Test ()
test =
  tests
    [ Term.test,
      TermParser.test,
      TermPrinter.test,
      Type.test,
      TypeError.test,
      TypePrinter.test,
      UnisonSources.test,
      FileParser.test,
      DataDeclaration.test,
      Text.test,
      Relation.test,
      Path.test,
      Causal.test,
      Referent.test,
      ABT.test,
      ANF.test,
      MCode.test,
      Var.test,
      Typechecker.test,
      Context.test,
      Name.test,
      PinBoard.test,
      CodebaseInit.test,
      Branch.test
    ]

main :: IO ()
main = withCP65001 do
  args <- getArgs
  mapM_ (`hSetEncoding` utf8) [stdout, stdin, stderr]
  case args of
    [] -> runOnly "" test
    [prefix] -> runOnly prefix test
    [seed, prefix] -> rerunOnly (read seed) prefix test
