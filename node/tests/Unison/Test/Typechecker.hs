{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Typechecker where

import Control.Applicative
import Unison.Note
import Unison.Term as E
import Unison.Type as T
import Unison.Typechecker as Typechecker
import Unison.Reference as R

import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

infixr 1 -->
(-->) :: Type -> Type -> Type
(-->) = T.arrow

data StrongEq = StrongEq Type
instance Eq StrongEq where StrongEq t1 == StrongEq t2 = Typechecker.equals t1 t2
instance Show StrongEq where show (StrongEq t) = show t

synthesizes :: Term -> Type -> Assertion
synthesizes e t =
  case (run (Typechecker.synthesize env e)) :: Either Note Type of
    Left err -> assertFailure ("synthesis failure: " ++ show err)
    Right t2 -> assertEqual "synthesis" (StrongEq t) (StrongEq t2)

checks :: Term -> Type -> Assertion
checks e t =
  case (run (Typechecker.check env e t)) :: Either Note Type of
    Left err -> assertFailure ("checking failure: " ++ show err)
    Right t2 -> pure ()

checkSubtype :: Type -> Type -> Assertion
checkSubtype t1 t2 = case Typechecker.subtype t1 t2 of
  Left err -> assertFailure ("subtype failure:\n" ++ show err)
  Right t2 -> pure ()

synthesizesAndChecks :: Term -> Type -> Assertion
synthesizesAndChecks e t =
  synthesizes e t >> checks e t

tests :: TestTree
tests = testGroup "Typechecker"
  [ testCase "alpha equivalence (term)" $ assertEqual "identity"
      (lam' ["a"] $ var' "a")
      (lam' ["x"] $ var' "x")
  , testCase "alpha equivalence (type)" $ assertEqual "const"
      (forall' ["a", "b"] $ T.v' "a" --> T.v' "b" --> T.v' "a")
      (forall' ["x", "y"] $ T.v' "x" --> T.v' "y" --> T.v' "x")
  , testCase "subtype (1)" $ checkSubtype
      (T.lit T.Number)
      (T.lit T.Number)
  , testCase "subtype (2)" $ checkSubtype
      (forall' ["a"] $ T.v' "a")
      (T.lit T.Number)
  , testCase "subtype (3)" $ checkSubtype
      (forall' ["a"] $ T.v' "a")
      (forall' ["a"] $ T.v' "a")
  , testCase "strong equivalence (type)" $ assertEqual "types were not equal"
      (StrongEq (forall' ["a", "b"] $ T.v' "a" --> T.v' "b" --> T.v' "a"))
      (StrongEq (forall' ["y", "x"] $ T.v' "x" --> T.v' "y" --> T.v' "x"))
  , testCase "synthesize 42" $ synthesizesAndChecks
      (E.lit (E.Number 42))
      (T.lit T.Number)
  , testCase "synthesize/check (x -> x)" $ synthesizesAndChecks
      (lam' ["a"] $ var' "a")
      (forall' ["b"] $ T.v' "b" --> T.v' "b")
  , testCase "synthesize/check (x y -> x)" $ synthesizesAndChecks
      (lam' ["x", "y"] $ var' "x")
      (forall' ["a", "b"] $ T.v' "a" --> T.v' "b" --> T.v' "a")
  , testCase "synthesize/check (let rec fix f = f (fix f) in fix)" $ synthesizesAndChecks
      (letRec' [("fix", lam' ["f"] $ var' "f" `E.app` (var' "fix" `E.app` var' "f"))] (var' "fix"))
      (forall' ["a"] $ (T.v' "a" --> T.v' "a") --> T.v' "a")
  , testCase "synthesize/check (let rec ping x = pong (x + 1); pong x = ping (x - 1) in ping 42)" $ synthesizesAndChecks
      (letRec'
        [ ("ping", lam' ["x"] $ var' "pong" `E.app` (plus (var' "x") (E.num 1))),
          ("pong", lam' ["y"] $ var' "pong" `E.app` (minus (var' "y") (E.num 1)))
        ]
        (var' "ping" `E.app` E.num 42))
      (T.lit T.Number)
  ]

plus :: Term -> Term -> Term
plus a b = E.ref (R.Builtin "+") `E.app` a `E.app` b

minus :: Term -> Term -> Term
minus a b = E.ref (R.Builtin "-") `E.app` a `E.app` b

env :: Applicative f => T.Env f
env r =
  let
    view a = T.app (T.ref (R.Builtin "View")) a
    numT =  T.lit T.Number
  in pure $ case r of
    Builtin "Color.rgba" -> numT --> numT --> numT --> numT --> T.ref (R.Builtin "Color")
    Builtin "+" -> numT --> numT --> numT
    Builtin "-" -> numT --> numT --> numT
    Builtin "View.view" -> forall' ["a"] $ view (T.v' "a") --> T.v' "a" --> T.v' "a"
    _ -> error $ "no type for reference " ++ show r

main :: IO ()
main = defaultMain tests
