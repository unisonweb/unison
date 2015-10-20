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

-- term for testing
type TTerm = Term (Symbol ())
type DTerm = Term (Symbol DFO)

hash :: TTerm -> Hash
hash e = ABT.hash e

dhash :: DTerm -> Hash
dhash e = ABT.hash e

tests :: IO TestTree
tests = do
  node <- MemNode.make
  symbols <- liftIO . Note.run $
    Map.fromList . Node.references <$> Node.search node blank [] 1000 (Metadata.Query "") Nothing
  let firstName (Metadata.Names (n:_)) = n
  let lookupSymbol ref = maybe (defaultSymbol ref) (firstName . Metadata.names) (Map.lookup ref symbols)
  let termDoc = view lookupSymbol
  pure $ testGroup "Term"
    [ testCase "alpha equivalence (term)" $ assertEqual "identity"
       ((lam' ["a"] $ var' "a") :: TTerm)
        (lam' ["x"] $ var' "x")
    , testCase "hash cycles" $ assertEqual "pingpong"
       (hash pingpong1)
       (hash pingpong2)
    , testCase "infix-rendering" $
      let t = builtin "Number.plus" `app` num 1 `app` num 1 :: DTerm
      in assertEqual "+"
           "1 + 1"
           (Doc.formatText (Width 80) (termDoc t))
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
plus a b = ref (R.Builtin "+") `app` a `app` b

minus :: TTerm -> TTerm -> TTerm
minus a b = ref (R.Builtin "-") `app` a `app` b

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
