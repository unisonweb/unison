{-# LANGUAGE OverloadedStrings #-}

module Unison.Test.Typechecker where

import Control.Applicative
import Unison.Node.MemNode ()
import Unison.Note
import Unison.Term as E
import Unison.Type as T
import Unison.Typechecker as Typechecker
import Unison.Reference as R
import Unison.Symbol (Symbol)
import qualified Unison.Test.Term as Term

import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

type TTerm = Term.TTerm
type TType = Type (Symbol ())
type TEnv f = T.Env f (Symbol ())

infixr 1 -->
(-->) :: TType -> TType -> TType
(-->) = T.arrow

data StrongEq = StrongEq TType
instance Eq StrongEq where StrongEq t1 == StrongEq t2 = Typechecker.equals t1 t2
instance Show StrongEq where show (StrongEq t) = show t

synthesizes :: TTerm -> TType -> Assertion
synthesizes e t =
  let
    handle r = case r of
      Left err -> assertFailure ("synthesis failure: " ++ show err)
      Right _ -> pure ()
  in
    handle $ do
      t2 <- (run (Typechecker.synthesize env e)) :: Either Note TType
      _ <- Typechecker.subtype t2 t
      _ <- Typechecker.subtype t t2
      pure ()

checks :: TTerm -> TType -> Assertion
checks e t =
  case (run (Typechecker.check env e t)) :: Either Note TType of
    Left err -> assertFailure ("checking failure: " ++ show err)
    Right t2 -> pure ()

checkSubtype :: TType -> TType -> Assertion
checkSubtype t1 t2 = case Typechecker.subtype t1 t2 of
  Left err -> assertFailure ("subtype failure:\n" ++ show err)
  Right t2 -> pure ()

synthesizesAndChecks :: TTerm -> TType -> Assertion
synthesizesAndChecks e t =
  synthesizes e t >> checks e t

tests :: IO TestTree
tests = pure $ testGroup "Typechecker"
  [ testCase "alpha equivalence (type)" $ assertEqual "const"
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
  , testCase "synthesize/check 42" $ synthesizesAndChecks
      (E.lit (E.Number 42))
      (T.lit T.Number)
  , testCase "synthesize/check Term.id" $ synthesizesAndChecks
      Term.id
      (forall' ["b"] $ T.v' "b" --> T.v' "b")
  , testCase "synthesize/check Term.const" $ synthesizesAndChecks
      Term.const
      (forall' ["a", "b"] $ T.v' "a" --> T.v' "b" --> T.v' "a")
  , testCase "synthesize/check (let f = (+) in f 1)" $ synthesizesAndChecks
      (let1' [("f", E.ref (R.Builtin "+"))] (var' "f" `E.app` E.num 1))
      (T.lit T.Number --> T.lit T.Number)
  , testCase "synthesize/check (let blank x = _ in blank 1)" $ synthesizesAndChecks
      (let1' [("blank", lam' ["x"] E.blank )] (var' "blank" `E.app` E.num 1))
      (forall' ["a"] $ T.v' "a")
  , testCase "synthesize/check Term.fix" $ synthesizesAndChecks
      Term.fix
      (forall' ["a"] $ (T.v' "a" --> T.v' "a") --> T.v' "a")
  , testCase "synthesize/check Term.pingpong1" $ synthesizesAndChecks
      Term.pingpong1
      (forall' ["a"] $ T.v' "a")
  ]

env :: Applicative f => TEnv f
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
main = defaultMain =<< tests
