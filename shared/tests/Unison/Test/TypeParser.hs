{-# LANGUAGE OverloadedStrings #-}
module Unison.Test.TypeParser where

import           Test.Tasty
import           Unison.Parser      (Result(..))
import           Unison.Parsers     (parseType)
import           Unison.Type        (Type)
import qualified Unison.Type        as T
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import           Test.Tasty.HUnit

import           Unison.Symbol      (Symbol)
import           Unison.View        (DFO)

parseV :: (String, Type (Symbol DFO)) -> TestTree
parseV (s,expected) =
  testCase ("`" ++ s ++ "`") $ case parseType s of
    Fail _ _ -> assertFailure "parse failure"
    Succeed a _ _ -> assertEqual "mismatch" expected a

tests :: TestTree
tests = testGroup "TypeParser" $ fmap parseV strings
  where
    strings :: [(String, Type (Symbol DFO))]
    strings =
      [ ("Number", T.lit T.Number)
      , ("Text", T.lit T.Text)
      , ("Vector", T.lit T.Vector)
      , ("Remote", T.builtin "Remote")
      , ("Foo", foo)
      , ("Foo -> Foo", T.arrow foo foo)
      , ("a -> a", T.arrow a a)
      , ("Foo -> Foo -> Foo", T.arrow foo (T.arrow foo foo))
      , ("Foo -> (Foo -> Foo)", T.arrow foo (T.arrow foo foo))
      , ("(Foo -> Foo) -> Foo", T.arrow (T.arrow foo foo) foo)
      , ("Vector Foo", T.vectorOf foo)
      , ("forall a . a -> a", forall_aa)
      , ("forall a. a -> a", forall_aa)
      , ("(forall a . a) -> Number", T.forall' ["a"] (T.v' "a") `T.arrow` T.lit T.Number)
      ]
    a = T.v' "a"
    foo = T.v' "Foo"
    forall_aa = T.forall' ["a"] (T.arrow a a)

main :: IO ()
main = defaultMain tests
