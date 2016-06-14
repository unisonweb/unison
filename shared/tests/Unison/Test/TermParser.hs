module Unison.Test.TermParser where

import           Test.Tasty
import           Unison.Dimensions
import           Unison.Parser
import           Unison.Term        (Term)
import qualified Unison.Term        as Term
import           Unison.TermParser
import           Unison.Type        (Type)
import qualified Unison.Type        as Type
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import qualified Data.Text          as Text
import           Test.Tasty.HUnit
import qualified Unison.Reference   as Reference
import qualified Unison.Test.Common as Common

import           Unison.Symbol      (Symbol)
import           Unison.View        (DFO)

parseV :: Parser (Term (Symbol DFO)) -> String -> TestTree
parseV p s =
  testCase ("`" ++ s ++ "`") $ case run p s of
    Fail _ _ -> assertFailure "parse failure"
    Succeed a n _ -> assertEqual (show (n - s') ++ " characters unparsed") n s'
    where s' = length s

parseV' :: Parser (Term (Symbol DFO)) -> (String, Term (Symbol DFO)) -> TestTree
parseV' p (s,expected) =
  testCase ("`" ++ s ++ "`") $ case run p s of
    Fail _ _ -> assertFailure "parse failure"
    Succeed a n _ -> assertEqual "mismatch" expected a


tests :: TestTree
tests = testGroup "TermParser" $ fmap (parseV term) strings
  where
    strings =
      [ "1 : Int"
      , "1:Int"
      , "(1:Int)"
      , "(1: Int)"
      , "(1: Int) : Int"
      , "add a b"
      , "a -> 3"
      , "a b -> add a b"
      , "let a = b in b -> a"
      , "let a : Int -> Int -> Int; a b c = b + c in a 1 2"
      , "let a = b in b -> a : a -> a"
      , "let a b c = b + c in a 1 2"
      , "let a + c = foo a c in 1 + 2"
      , "let a = b in c"
      , "let a = b; c = d in c"
      , "let rec a = b in c"
      , "let rec a = b; c = d in c"
      , "a b c"
      , "1 2 3"
      , "(a b) (c d)"
      , "(let rec a = b; c = d in c) a b"
      ]

tests2 :: TestTree
tests2 = testGroup "TermParser2" $ fmap (parseV' term) strings
  where
    strings =
      [ ("1", one)
      , ("[1,1]", Term.vector [one, one])
      , ("[1,1] : Vector Number", Term.ann (Term.vector [one, one]) (Type.vectorOf number))
      , ("\"hello\"", hello)
      , ("_", blank)
      , ("1:Int", Term.ann one int)
      , ("(1:Int)", Term.ann one int)
      , ("(1:Int) : Int", Term.ann (Term.ann one int) int)
      , ("let a = 1 in a + 1", Term.let1'' [("a", one)] (Term.apps plus [a, one]))
      , ("let a : Int; a = 1 in a + 1", Term.let1'' [("a", one)] (Term.apps plus [a,one]))
      , ("a b -> a + b", lam_ab_aplusb)
      , ("(a b -> a + b) : Int -> Int -> Int", Term.ann lam_ab_aplusb (Type.arrow int (Type.arrow int int)))
      , ("a b -> a + b : Int", Term.lam''' ["a", "b"] (Term.ann (Term.apps plus [a, b]) int))
      , ("a -> a", Term.lam''' ["a"] a)
      , ("(a -> a) : forall a . a -> a", Term.ann (Term.lam''' ["a"] a) (Type.forall'' ["a"] (Type.arrow a' a')))
      , ("let f = a b -> a + b in f 1 1", f_eq_lamab_in_f11)
      , ("let f a b = a + b in f 1 1", f_eq_lamab_in_f11)
      , ("let f (+) b = 1 + b in f g 1", Term.let1'' [("f", Term.lam''' ["+", "b"] (Term.apps plus [one, b]))] (Term.apps f [g,one]))
      , ("let a + b = f a b in 1 + 1", Term.let1'' [("+", Term.lam''' ["a", "b"] (Term.apps f [a, b]))] (Term.apps plus [one,one]))
      , ("let f b = b + 1; a = 1 in (+) a (f 1)", Term.let1'' [("f", lam_b_bplus1), ("a", one)] (Term.apps plus [a, Term.apps f [one]]))
      ]
    one = (Term.lit . Term.Number) 1
    hello = (Term.lit . Term.Text . Text.pack) "hello"
    blank = Term.blank
    number :: Ord v => Type v
    number = Type.lit Type.Number
    int = Type.v'' "Int"
    a = Term.var'' "a"
    a' = Type.v'' "a"
    b = Term.var'' "b"
    f = Term.var'' "f"
    g = Term.var'' "g"
    plus = Term.var'' "+"
    lam_ab_aplusb = Term.lam''' ["a", "b"] (Term.apps plus [a, b])
    lam_b_bplus1 = Term.lam''' ["b"] (Term.apps plus [b, one])
    f_eq_lamab_in_f11 = Term.let1'' [("f", lam_ab_aplusb)] (Term.apps f [one,one])

main = defaultMain tests2 -- >> defaultMain tests
{-
module Unison.Test.TermParser where

import           Test.Tasty
import           Unison.Dimensions
import           Unison.Parser
import           Unison.Term        (Term)
import qualified Unison.Term        as Term
import           Unison.TermParser
import           Unison.Type        (Type)
import qualified Unison.Type        as Type
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import qualified Data.Text          as Text
import           Test.Tasty.HUnit
import qualified Unison.Reference   as Reference
import qualified Unison.Test.Common as Common

import           Unison.Symbol      (Symbol)
import           Unison.View        (DFO)

parseV :: Parser (Term (Symbol DFO)) -> String -> TestTree
parseV p s =
  testCase ("`" ++ s ++ "`") $ case run p s of
    Fail _ _ -> assertFailure "parse failure"
    Succeed a n _ -> assertEqual (show (n - s') ++ " characters unparsed") n s'
    where s' = length s

parseV' :: Parser (Term (Symbol DFO)) -> (String, Term (Symbol DFO)) -> TestTree
parseV' p (s,expected) =
  testCase ("`" ++ s ++ "`") $ case run p s of
    Fail _ _ -> assertFailure "parse failure"
    Succeed a n _ -> assertEqual "mismatch" expected a


tests :: TestTree
tests = testGroup "TermParser" $ fmap (parseV term) strings
  where
    strings =
      [ "1 : Int"
      , "1:Int"
      , "(1:Int)"
      , "(1: Int)"
      , "(1: Int) : Int"
      , "add a b"
      , "a -> 3"
      , "a b -> add a b"
      , "let a = b in b -> a"
      , "let a : Int -> Int -> Int; a b c = b + c in a 1 2"
      , "let a = b in b -> a : a -> a"
      , "let a b c = b + c in a 1 2"
      , "let a : Int -> Int -> Int; a b c = b + c in a 1 2"
      , "let a + c = foo a c in 1 + 2"
      , "let a = b in c"
      , "let a = b; c = d in c"
      , "let rec a = b in c"
      , "let rec a = b; c = d in c"
      , "a b c"
      , "1 2 3"
      , "(a b) (c d)"
      , "(let rec a = b; c = d in c) a b"
      ]

tests2 :: TestTree
tests2 = testGroup "TermParser2" $ fmap (parseV' term) strings
  where
    strings =
      [ ("1", one)
      , ("[1,1]", Term.vector [one, one])
      , ("[1,1] : Vector Number", Term.ann (Term.vector [one, one]) (Type.vectorOf number))
      , ("\"hello\"", hello)
      , ("_", blank)
      , ("1:Int", Term.ann one int)
      , ("(1:Int)", Term.ann one int)
      , ("(1:Int) : Int", Term.ann (Term.ann one int) int)
      , ("let a = 1 in a + 1", Term.let1'' [("a", one)] (Term.apps plus [a, one]))
      , ("let a : Int; a = 1 in a + 1", Term.let1'' [("a", one)] (Term.apps plus [a,one]))
      , ("a b -> a + b", lam_ab_aplusb)
      , ("(a b -> a + b) : Int -> Int -> Int", Term.ann lam_ab_aplusb intintint)
      , ("a b -> a + b : Int", Term.lam''' ["a", "b"] (Term.ann (Term.apps plus [a, b]) int))
      , ("a -> a", Term.lam''' ["a"] a)
      , ("(a -> a) : forall a . a -> a", Term.ann (Term.lam''' ["a"] a) (Type.forall'' ["a"] (Type.arrow a' a')))
      , ("let f = a b -> a + b in f 1 1", f_eq_lamab_in_f11)
      , ("let f a b = a + b in f 1 1", f_eq_lamab_in_f11)
      , ("let a + b = f a b in 1 + 1", Term.let1'' [("+", Term.lam''' ["a", "b"] fab)] one_plus_one)
      , ("let (+) : Int -> Int -> Int; a + b = f a b in 1 + 1", plusintintint_fab_in_1plus1)
      , ("let (+) : Int -> Int -> Int; (+) a b = f a b in 1 + 1", plusintintint_fab_in_1plus1)
      , ("let (+) : Int -> Int -> Int; (+) a b = f a b in (+) 1 1", plusintintint_fab_in_1plus1)
      , ("let f b = b + 1; a = 1 in (+) a (f 1)", Term.let1'' [("f", lam_b_bplus1), ("a", one)] (Term.apps plus [a, Term.apps f [one]]))
      ]
    one = (Term.lit . Term.Number) 1
    hello = (Term.lit . Term.Text . Text.pack) "hello"
    blank = Term.blank
    number :: Ord v => Type v
    number = Type.lit Type.Number
    int = Type.v'' "Int"
    intintint = Type.arrow int (Type.arrow int int)
    plusintintint_fab_in_1plus1 = Term.let1'' [("+", Term.ann lam_ab_fab intintint)] one_plus_one
    a = Term.var'' "a"
    a' = Type.v'' "a"
    b = Term.var'' "b"
    f = Term.var'' "f"
    g = Term.var'' "g"
    plus = Term.var'' "+"
    one_plus_one = Term.apps plus [one,one]
    lam_ab_aplusb = Term.lam''' ["a", "b"] (Term.apps plus [a, b])
    lam_ab_fab = Term.lam''' ["a", "b"] fab
    fab = Term.apps f [a, b]
    lam_b_bplus1 = Term.lam''' ["b"] (Term.apps plus [b, one])
    f_eq_lamab_in_f11 = Term.let1'' [("f", lam_ab_aplusb)] (Term.apps f [one,one])

main = defaultMain tests2 -- >> defaultMain tests
-}
