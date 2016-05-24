module Unison.Test.TermParser where

import Unison.Term (Term)
import Unison.TermParser
import Unison.Parser
import Test.Tasty
import Unison.Dimensions
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import qualified Unison.Test.Common as Common
import qualified Unison.Reference as Reference
import qualified Data.Text as Text

import Unison.Symbol (Symbol)
import Unison.View (DFO)

parseV :: Parser (Term (Symbol DFO)) -> String -> TestTree
parseV p s =
  testCase ("`" ++ s ++ "`") $ case run p s of
    Fail _ _ -> assertFailure "parse failure"
    Succeed a n -> assertEqual (show (n - s') ++ " characters unparsed") n s'
      where s' = length s

tests :: TestTree
tests = testGroup "TermParser" $ fmap (parseV $ term l) strings
  where
     l s = Just (Reference.Builtin $ Text.pack s)
     strings = [ "1 : Int"
                , "1:Int"
                , "(1: Int) : Int"
                , "add a b"
                , "a -> 3"
                , "a b -> add a b"
                , "let a = b in b -> a"
                , "let a = b in b -> a : a -> a"
                , "let a = b in c"
                , "let a = b; c = d in c"
                , "let rec a = b in c"
                , "let rec a = b; c = d in c"
                , "a b c"
                , "1 2 3"
                , "(a b) (c d)"
                , "(let rec a = b; c = d in c) a b"
                ]

main = defaultMain tests
