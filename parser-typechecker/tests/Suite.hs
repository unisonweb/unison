{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Main where

import EasyTest
import System.Environment (getArgs)
import System.IO
import System.IO.CodePage (withCP65001)
import Unison.Core.Test.Name qualified as Name
import Unison.Test.ABT qualified as ABT
import Unison.Test.Codebase.Branch qualified as Branch
import Unison.Test.Codebase.Causal qualified as Causal
import Unison.Test.Codebase.Givens qualified as Givens
import Unison.Test.Codebase.Path qualified as Path
import Unison.Test.CodebaseInit qualified as CodebaseInit
import Unison.Test.DataDeclaration qualified as DataDeclaration
import Unison.Test.Referent qualified as Referent
import Unison.Test.Syntax.FileParser qualified as FileParser
import Unison.Test.Syntax.TermParser qualified as TermParser
import Unison.Test.Syntax.TypePrinter qualified as TypePrinter
import Unison.Test.Term qualified as Term
import Unison.Test.Type qualified as Type
import Unison.Test.Typechecker qualified as Typechecker
import Unison.Test.Typechecker.Context qualified as Context
import Unison.Test.Typechecker.GivenApply qualified as GivenApply
import Unison.Test.Typechecker.GivenBindings qualified as GivenBindings
import Unison.Test.Typechecker.GivenCore qualified as GivenCore
import Unison.Test.Typechecker.GivenFinish qualified as GivenFinish
import Unison.Test.Typechecker.GivenGoal qualified as GivenGoal
import Unison.Test.Typechecker.GivenGoals qualified as GivenGoals
import Unison.Test.Typechecker.GivenLexical qualified as GivenLexical
import Unison.Test.Typechecker.GivenParameters qualified as GivenParameters
import Unison.Test.Typechecker.GivenPlan qualified as GivenPlan
import Unison.Test.Typechecker.GivenResolver qualified as GivenResolver
import Unison.Test.Typechecker.GivenSites qualified as GivenSites
import Unison.Test.Typechecker.GivenTdnr qualified as GivenTdnr
import Unison.Test.Typechecker.TypeError qualified as TypeError
import Unison.Test.Util.Relation qualified as Relation
import Unison.Test.Util.Text qualified as Text
import Unison.Test.Var qualified as Var

test :: Test ()
test =
  tests
    [ Term.test,
      TermParser.test,
      Type.test,
      TypeError.test,
      TypePrinter.test,
      FileParser.test,
      DataDeclaration.test,
      Text.test,
      Relation.test,
      Path.test,
      Causal.test,
      Givens.test,
      Referent.test,
      ABT.test,
      Var.test,
      Typechecker.test,
      Context.test,
      GivenApply.test,
      GivenBindings.test,
      GivenCore.test,
      GivenFinish.test,
      GivenGoal.test,
      GivenGoals.test,
      GivenLexical.test,
      GivenParameters.test,
      GivenPlan.test,
      GivenResolver.test,
      GivenSites.test,
      GivenTdnr.test,
      Name.test,
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
