{-# LANGUAGE OverloadedStrings #-}
module Unison.Test.Term where

import Unison.Term
import Unison.Term.Extra ()
import Unison.Reference as R
import Test.Tasty
-- import Test.Tasty.SmallCheck as SC
-- import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import qualified Unison.ABT.Extra as ABT

tests :: TestTree
tests = testGroup "Typechecker"
  [ testCase "alpha equivalence (term)" $ assertEqual "identity"
     (lam' ["a"] $ var' "a")
     (lam' ["x"] $ var' "x")
  , testCase "hash cycles" $ assertEqual "pingpong"
     (ABT.hash pingpong1)
     (ABT.hash pingpong2)
  ]

-- various unison terms, useful for testing

id :: Term
id = lam' ["a"] $ var' "a"

const :: Term
const = lam' ["x", "y"] $ var' "x"

one :: Term
one = num 1

zero :: Term
zero = num 0

plus :: Term -> Term -> Term
plus a b = ref (R.Builtin "+") `app` a `app` b

minus :: Term -> Term -> Term
minus a b = ref (R.Builtin "-") `app` a `app` b

fix :: Term
fix = letRec'
  [ ("fix", lam' ["f"] $ var' "f" `app` (var' "fix" `app` var' "f")) ]
  (var' "fix")

pingpong1 :: Term
pingpong1 =
  letRec'
    [ ("ping", lam' ["x"] $ var' "pong" `app` (plus (var' "x") one))
    , ("pong", lam' ["y"] $ var' "pong" `app` (minus (var' "y") one)) ]
    (var' "ping" `app` one)

pingpong2 :: Term
pingpong2 =
  letRec'
    [ ("pong1", lam' ["p"] $ var' "pong1" `app` (minus (var' "p") one))
    , ("ping1", lam' ["q"] $ var' "pong1" `app` (plus (var' "q") one)) ]
    (var' "ping1" `app` one)
