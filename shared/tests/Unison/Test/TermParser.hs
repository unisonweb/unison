{-# LANGUAGE OverloadedStrings #-}
module Unison.Test.TermParser where

import           Test.Tasty
import           Unison.Parser
import           Unison.Parsers (parseTerm)
import           Unison.Term
import           Unison.Type        (Type)
import qualified Unison.Type        as Type
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import qualified Data.Text          as Text
import           Test.Tasty.HUnit

import           Unison.Symbol      (Symbol)
import           Unison.View        (DFO)

parse :: (String, Term (Symbol DFO)) -> TestTree
parse (s, expected) =
  testCase ("`" ++ s ++ "`") $
    case parseTerm s of
      Fail _ _ -> assertFailure "parse failure"
      Succeed a _ _ -> assertEqual "mismatch" expected a

tests :: TestTree
tests = testGroup "TermParser" $ parse <$> strings
  where
    strings =
      [ ("1", one)
      , ("[1,1]", vector [one, one])
      , ("[1,1] : Vector Number", ann (vector [one, one]) (Type.vectorOf number))
      , ("[1+1]", vector [one_plus_one])
      , ("\"hello\"", hello)
      , ("_", blank)
      , ("a", a)
      , ("Number.plus", numberplus)
      , ("1:Int", ann one int)
      , ("(1:Int)", ann one int)
      , ("(1:Int) : Int", ann (ann one int) int)
      , ("let a = 1 in a + 1", let1' [("a", one)] (apps numberplus [a, one]))
      , ("let a : Int; a = 1 in a + 1", let_a_int1_in_aplus1)
      , ("let a: Int; a = 1 in a + 1", let_a_int1_in_aplus1)
      , ("let a :Int; a = 1 in a + 1", let_a_int1_in_aplus1)
      , ("let a:Int; a = 1 in a + 1", let_a_int1_in_aplus1)
      , ("a b -> a + b", lam_ab_aplusb)
      , ("(a b -> a + b) : Int -> Int -> Int", ann lam_ab_aplusb intintint)
      , ("a b -> a + b : Int", lam' ["a", "b"] (ann (apps numberplus [a, b]) int))
      , ("a -> a", lam' ["a"] a)
      , ("(a -> a) : forall a . a -> a", ann (lam' ["a"] a) (Type.forall' ["a"] (Type.arrow a' a')))
      , ("let f = a b -> a + b in f 1 1", f_eq_lamab_in_f11)
      , ("let f a b = a + b in f 1 1", f_eq_lamab_in_f11)
      , ("let f (+) b = 1 + b in f g 1", let1' [("f", lam' ["+", "b"] (apps plus [one, b]))] (apps f [g,one]))
      , ("let a + b = f a b in 1 + 1", let1' [("+", lam' ["a", "b"] fab)] one_plus_one)
      , ("let (+) : Int -> Int -> Int; a + b = f a b in 1 + 1", plusintintint_fab_in_1plus1)
      , ("let (+) : Int -> Int -> Int; (+) a b = f a b in 1 + 1", plusintintint_fab_in_1plus1)
      , ("let (+) : Int -> Int -> Int; (+) a b = f a b in (+) 1 1", plusintintint_fab_in_1plus1)
      , ("let f b = b + 1; a = 1 in (+) a (f 1)", let1' [("f", lam_b_bplus1), ("a", one)] (apps numberplus [a, apps f [one]]))
      -- from Unison.Test.Term
      , ("a -> a", lam' ["a"] $ var' "a") -- id
      , ("x y -> x", lam' ["x", "y"] $ var' "x") -- const
      , ("let rec fix = f -> f (fix f) in fix", fix) -- fix
      , ("let rec fix f = f (fix f) in fix", fix) -- fix
      , ("1 + 2 + 3", num 1 `plus'` num 2 `plus'` num 3)
      , ("[1, 2, 1 + 1]", vector [num 1, num 2, num 1 `plus'` num 1])
      ]
    one = (lit . Number) 1
    hello = (lit . Text . Text.pack) "hello"
    number :: Ord v => Type v
    number = Type.lit Type.Number
    int = Type.v' "Int"
    intintint = Type.arrow int (Type.arrow int int)
    a = var' "a"
    a' = Type.v' "a"
    b = var' "b"
    f = var' "f"
    g = var' "g"
    plus = var' "+"
    plus' x y = builtin "Number.plus" `app` x `app` y
    numberplus = builtin "Number.plus"
    one_plus_one = apps plus [one,one]
    lam_ab_aplusb = lam' ["a", "b"] (apps numberplus [a, b])
    lam_b_bplus1 = lam' ["b"] (apps numberplus [b, one])
    lam_ab_fab = lam' ["a", "b"] fab
    fab = apps f [a, b]
    f_eq_lamab_in_f11 = let1' [("f", lam_ab_aplusb)] (apps f [one,one])
    let_a_int1_in_aplus1 = let1' [("a", ann one int)] (apps numberplus [a,one])
    plusintintint_fab_in_1plus1 = let1' [("+", ann lam_ab_fab intintint)] one_plus_one
    fix = letRec'
        [ ("fix", lam' ["f"] $ var' "f" `app` (var' "fix" `app` var' "f")) ]
        (var' "fix")

main :: IO ()
main = defaultMain tests
