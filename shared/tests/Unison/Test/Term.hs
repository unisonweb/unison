{-# LANGUAGE OverloadedStrings #-}
module Unison.Test.Term where

-- import Test.Tasty.QuickCheck as QC
-- import Test.Tasty.SmallCheck as SC
import Control.Monad.IO.Class
import Test.Tasty
import Test.Tasty.HUnit
import Unison.Hash (Hash)
import Unison.Node.MemNode ()
import Unison.Reference as R
import Unison.Symbol (Symbol)
import Unison.Term
import Unison.Type (defaultSymbol)
import Unison.Var (Var)
import Unison.View (DFO)
import Unison.Dimensions (Width(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Doc as Doc
import qualified Unison.Metadata as Metadata
import qualified Unison.Node as Node
import qualified Unison.Node.MemNode as MemNode
import qualified Unison.Note as Note
import qualified Unison.Test.Common as Common

-- term for testing
type TTerm = Term (Symbol DFO)

hash :: TTerm -> Hash
hash e = ABT.hash e

tests :: TestTree
tests = withResource Common.node (\_ -> pure ()) $ \node -> testGroup "Term"
    [ testCase "alpha equivalence (term)" $ assertEqual "identity"
       ((lam' ["a"] $ var' "a") :: TTerm)
        (lam' ["x"] $ var' "x")
    , testCase "hash cycles" $ assertEqual "pingpong"
       (hash pingpong1)
       (hash pingpong2)
    , testCase "infix-rendering" $ node >>= \(_,symbol) ->
        let t = num 1 `plus` num 1
        in assertEqual "+"
          "1 + 1"
          (Doc.formatText (Width 80) (view symbol t))
    ]

-- various unison terms, useful for testing

id :: TTerm
id = lam' ["a"] $ var' "a"

const :: TTerm
const = lam' ["x", "y"] $ var' "x"

one :: TTerm
one = num 1

zero :: TTerm
zero = num 0

plus :: TTerm -> TTerm -> TTerm
plus a b = builtin "Number.plus" `app` a `app` b

minus :: TTerm -> TTerm -> TTerm
minus a b = builtin "Number.minus" `app` a `app` b

fix :: TTerm
fix = letRec'
  [ ("fix", lam' ["f"] $ var' "f" `app` (var' "fix" `app` var' "f")) ]
  (var' "fix")

pingpong1 :: TTerm
pingpong1 =
  letRec'
    [ ("ping", lam' ["x"] $ var' "pong" `app` (plus (var' "x") one))
    , ("pong", lam' ["y"] $ var' "pong" `app` (minus (var' "y") one)) ]
    (var' "ping" `app` one)

pingpong2 :: TTerm
pingpong2 =
  letRec'
    [ ("pong1", lam' ["p"] $ var' "pong1" `app` (minus (var' "p") one))
    , ("ping1", lam' ["q"] $ var' "pong1" `app` (plus (var' "q") one)) ]
    (var' "ping1" `app` one)
