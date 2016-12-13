module Unison.Test.SerializationAndHashing where

import Test.Tasty
import Test.Tasty.HUnit
import Unison.Parsers (unsafeParseTerm)
import qualified Unison.SerializationAndHashing as SAH

testTermString :: String -> Assertion
testTermString termString =
  let term = unsafeParseTerm termString :: SAH.TermV
      roundTrip = SAH.deserializeTermFromBytes . SAH.serializeTerm $ term
  in case roundTrip of
    Left s -> fail s
    Right t -> if t == term then pure () else fail (show t)

fortyTwo :: Assertion
fortyTwo = testTermString "42"

fortyTwoNum :: Assertion
fortyTwoNum = testTermString "42 : Number"

hiText :: Assertion
hiText = testTermString "\"hi\" : Text"

lambda :: Assertion
lambda = testTermString "x -> x"

letBinding :: Assertion
letBinding = testTermString "let { x = 42; x + 1 }"

letRec :: Assertion
letRec = testTermString "let rec { x = x + 1; x }"

vec :: Assertion
vec = testTermString "[\"a\", \"b\", \"c\"]"

tests :: TestTree
tests = testGroup "SerializationAndHashing"
  [ testCase "fortyTwo" fortyTwo
  , testCase "fortyTwoNum" fortyTwoNum
  , testCase "hiText" hiText
  , testCase "lambda" lambda
  , testCase "letBinding" letBinding
  , testCase "letRec" letRec
  , testCase "vec" vec
  ]
