module Unison.Test.Interpreter where

import Test.Tasty
import Test.Tasty.HUnit
import Unison.Parsers (unsafeParseTerm)
import qualified Unison.Node as Node
import qualified Unison.Note as Note
import qualified Unison.Test.Common as Common

tests :: TestTree
tests = withResource Common.node (\_ -> pure ()) $ \node ->
  let
    tests =
      [ evaluatesTo "1 + 1" "2"
      , evaluatesTo "1 + 1 + 1" "3"
      , evaluatesTo "(x -> x) 42" "42"
      , evaluatesTo "let x = 2; y = 3 in x + y" "5" ]
    evaluatesTo uneval eval = testCase (uneval ++ " == " ++ eval) $ do
      (node, _) <- node
      [(_,_,result)] <- Note.run $ Node.evaluateTerms node [([], unsafeParseTerm uneval)]
      assertEqual "comparing results" result (unsafeParseTerm eval)
  in testGroup "Interpreter" tests

main = defaultMain tests
