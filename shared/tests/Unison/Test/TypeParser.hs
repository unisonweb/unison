module Unison.Test.TypeParser where

import           Test.Tasty
import           Unison.Dimensions
import           Unison.Parser
import           Unison.Type        (Type)
import qualified Unison.Type        as Type
import           Unison.TypeParser
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import qualified Data.Text          as Text
import           Test.Tasty.HUnit
import qualified Unison.Reference   as Reference
import qualified Unison.Test.Common as Common

import           Unison.Symbol      (Symbol)
import           Unison.View        (DFO)

parseV :: Parser (Type (Symbol DFO)) -> (String, Type (Symbol DFO)) -> TestTree
parseV p (s,expected) =
  testCase ("`" ++ s ++ "`") $ case run p s of
    Fail _ _ -> assertFailure "parse failure"
    Succeed a n _ -> assertEqual "mismatch" expected a

tests :: TestTree
tests = testGroup "TermParser" $ fmap (parseV type_) strings
  where
    strings =
      [ ("Number", Type.lit Type.Number)
      , ("Text", Type.lit Type.Text)
      , ("Vector", Type.lit Type.Vector)
      , ("Foo", foo)
      , ("Foo -> Foo", Type.arrow foo foo)
      , ("a -> a", Type.arrow a a)
      , ("Foo -> Foo -> Foo", Type.arrow foo (Type.arrow foo foo))
      , ("Foo -> (Foo -> Foo)", Type.arrow foo (Type.arrow foo foo))
      , ("(Foo -> Foo) -> Foo", Type.arrow (Type.arrow foo foo) foo)
      , ("Vector Foo", Type.vectorOf foo)
      , ("forall a . a -> a", forall_aa)
      , ("forall a. a -> a", forall_aa)
      ]
    a = Type.v' $ Text.pack "a"
    -- b = Type.v' $ Text.pack "b"
    foo = Type.v' $ Text.pack "Foo"
    forall_aa = Type.forall'' ["a"] (Type.arrow a a)

main = defaultMain tests
