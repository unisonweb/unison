{-# LANGUAGE OverloadedStrings #-}
module Unison.Test.TermParser where

import Data.List
import Data.Text (Text)
import Test.Tasty
import Test.Tasty.HUnit
import Unison.Parser (Result(..))
import Unison.Parsers (parseTerm)
import Unison.Symbol (Symbol)
import Unison.Term
import Unison.Type (Type)
import Unison.View (DFO)
import qualified Data.Text as Text
import qualified Unison.Type as T
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC

parse' :: String -> TestTree
parse' s = testCase ("`" ++ s ++ "`") $
  case parseTerm s of
    Fail e _ -> assertFailure $ "parse failure " ++ intercalate "\n" e
    Succeed a _ _ -> pure ()

parse :: (String, Term (Symbol DFO)) -> TestTree
parse (s, expected) =
  testCase ("`" ++ s ++ "`") $
    case parseTerm s of
      Fail e _ -> assertFailure $ "parse failure " ++ intercalate "\n" e
      Succeed a _ _ -> assertEqual "mismatch" expected a

parseFail :: (String,String) -> TestTree
parseFail (s, reason) =
  testCase ("`" ++ s ++ "` shouldn't parse: " ++ reason) $ assertBool "should not have parsed" $
    case parseTerm s of
      Fail {} -> True;
      Succeed _ _ n -> n == length s;

tests :: TestTree
tests = testGroup "TermParser" $ (parse <$> shouldPass)
                              ++ (parse' <$> shouldParse)
                              ++ (parseFail <$> shouldFail)
  where
    shouldFail =
      [ ("+", "operator needs to be enclosed in parens or between arguments")
      , ("#V-fXHD3-N0E", "invalid base64url")
      , ("#V-f/XHD3-N0E", "invalid base64url")
      ]
    shouldParse =
      [ "do Remote n1 := Remote.spawn; n2 := Remote.spawn; let rec x = 10; Remote.pure 42;;; ;" ]
    shouldPass =
      [ ("1", one)
      , ("[1,1]", vector [one, one])
      , ("[1,1] : Vector Number", ann (vector [one, one]) (T.vectorOf number))
      , ("(+)", numberplus)
      , ("(++)", var' "++")
      , ("(++)", var' "++")
      , ("((++))", var' "++")
      , ("1+", var' "1+")
      , ("(1+)", var' "1+")
      , ("((1+))", var' "1+")
      , ("1+1", onenone)
      , ("1+1", onenone)
      , ("1+ 1", app (var' "1+") one)
      , ("1 +1", app one (var' "+1"))
      , ("[1+1]", vector [onenone])
      , ("\"hello\"", hello)
      , ("_", blank)
      , ("a", a)
      , ("Number.plus", numberplus)
      , ("Number.Other.plus", var' "Number.Other.plus")
      , ("f -> Remote.bind (#V-fXHD3-N0E= Remote.pure f)", remoteMap)
      , ("1:Int", ann one int)
      , ("(1:Int)", ann one int)
      , ("(1:Int) : Int", ann (ann one int) int)
      , ("let a = 1; a + 1;;", let1' [("a", one)] (apps numberplus [a, one]))
      , ("let a : Int; a = 1; a + 1;;", let_a_int1_in_aplus1)
      , ("let a: Int; a = 1; a + 1;;", let_a_int1_in_aplus1)
      , ("let a :Int; a = 1; a + 1;;", let_a_int1_in_aplus1)
      , ("let a:Int; a = 1; a + 1;;", let_a_int1_in_aplus1)
      , ("a b -> a + b", lam_ab_aplusb)
      , ("(a b -> a + b) : Int -> Int -> Int", ann lam_ab_aplusb intintint)
      , ("a b -> a + b : Int", lam' ["a", "b"] (ann (apps numberplus [a, b]) int))
      , ("a -> a", lam' ["a"] a)
      , ("(a -> a) : forall a . a -> a", ann (lam' ["a"] a) (T.forall' ["a"] (T.arrow a' a')))
      , ("let f = a b -> a + b; f 1 1;;", f_eq_lamab_in_f11)
      , ("let f a b = a + b; f 1 1;;", f_eq_lamab_in_f11)
      , ("let f (+) b = 1 + b; f g 1;;", let1' [("f", lam' ["+", "b"] (apps plus [one, b]))] (apps f [g,one]))
      , ("let a + b = f a b; 1 + 1;;", let1' [("+", lam' ["a", "b"] fab)] one_plus_one)
      , ("let (+) : Int -> Int -> Int; a + b = f a b; 1 + 1;;", plusintintint_fab_in_1plus1)
      , ("let (+) : Int -> Int -> Int; (+) a b = f a b; 1 + 1;;", plusintintint_fab_in_1plus1)
      , ("let (+) : Int -> Int -> Int; (+) a b = f a b; (+) 1 1;;", plusintintint_fab_in_1plus1)
      , ("let f b = b + 1; a = 1; (+) a (f 1);;", let1' [("f", lam_b_bplus1), ("a", one)] (apps numberplus [a, apps f [one]]))
      -- from Unison.Test.Term
      , ("a -> a", lam' ["a"] $ var' "a") -- id
      , ("x y -> x", lam' ["x", "y"] $ var' "x") -- const
      , ("let rec fix = f -> f (fix f); fix;;", fix) -- fix
      , ("let rec fix f = f (fix f); fix;;", fix) -- fix
      , ("1 + 2 + 3", num 1 `plus'` num 2 `plus'` num 3)
      , ("[1, 2, 1 + 1]", vector [num 1, num 2, num 1 `plus'` num 1])
      , ("(id -> let x = id 42; y = id \"hi\"; 43;;) : (forall a . a) -> Number", lam' ["id"] (let1'
        [ ("x", var' "id" `app` num 42),
          ("y", var' "id" `app` text "hi")
        ] (num 43)) `ann` (T.forall' ["a"] (T.v' "a") `T.arrow` T.lit T.Number))
        , ("#" ++ Text.unpack sampleHash64, derived' sampleHash64)
        , ("#" ++ Text.unpack sampleHash512, derived' sampleHash512)
      , ("(do Remote pure 42;;)", builtin "Remote.pure" `app` num 42)
      , ("do Remote x = 42; pure (x + 1) ;;",
          builtin "Remote.bind" `apps` [
            lam' ["q"] (builtin "Remote.pure" `app` (var' "q" `plus'` num 1)),
            builtin "Remote.pure" `app` num 42
          ]
        )
      , ("do Remote x := pure 42;  pure (x + 1) ;;",
          builtin "Remote.bind" `apps` [
            lam' ["q"] (builtin "Remote.pure" `app` (var' "q" `plus'` num 1)),
            builtin "Remote.pure" `app` num 42
          ]
        )
      , ("do Remote\n   x := pure 42;\n   y := pure 18;\n   pure (x + y);;",
          builtin "Remote.bind" `apps` [
            lam' ["x"] (builtin "Remote.bind" `apps` [
              lam' ["y"] (builtin "Remote.pure" `app` (var' "x" `plus'` var' "y")),
              builtin "Remote.pure" `app` num 18
            ]),
            builtin "Remote.pure" `app` num 42
          ]
        )
      ]
    one = (lit . Number) 1
    hello = text "hello"
    number :: Ord v => Type v
    number = T.lit T.Number
    int = T.v' "Int"
    intintint = T.arrow int (T.arrow int int)
    a = var' "a"
    a' = T.v' "a"
    b = var' "b"
    f = var' "f"
    g = var' "g"
    plus = var' "+"
    plus' x y = builtin "Number.plus" `app` x `app` y
    numberplus = builtin "Number.plus"
    remotepure = builtin "Remote.pure"
    remoteMap = lam' ["f"] (builtin "Remote.bind" `app` (derived' sampleHash64 `app` remotepure `app` var' "f"))
    onenone = var' "1+1"
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
    sampleHash64 = "V-fXHD3-N0E=" :: Text
    sampleHash512 = "1jgF5VUh1odeSCmmI94efghSPl3yAnopDCGeQC7qFkIcxLXKSJHxpvLcORW-mf1xMgXH-wigSVFmz83-acCllQ==" :: Text

main :: IO ()
main = defaultMain tests
