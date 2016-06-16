{-# LANGUAGE OverloadedStrings #-}
module Unison.Test.TypeParser where

import           Test.Tasty
import           Unison.Parser
import           Unison.Type        (Type)
import qualified Unison.Type        as Type
import           Unison.TypeParser
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import           Test.Tasty.HUnit

import           Unison.Symbol      (Symbol)
import           Unison.View        (DFO)

parseV :: Parser (Type (Symbol DFO)) -> (String, Type (Symbol DFO)) -> TestTree
parseV p (s,expected) =
  testCase ("`" ++ s ++ "`") $ case run p s of
    Fail _ _ -> assertFailure "parse failure"
    Succeed a _ _ -> assertEqual "mismatch" expected a

tests :: TestTree
tests = testGroup "TypeParser" $ fmap (parseV type_) strings
  where
    strings :: [(String, Type V)]
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
    a = Type.v' "a"
    foo = Type.v' "Foo"
    forall_aa = Type.forall' ["a"] (Type.arrow a a)

main :: IO ()
main = defaultMain tests
