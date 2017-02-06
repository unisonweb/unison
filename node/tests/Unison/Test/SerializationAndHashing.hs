module Unison.Test.SerializationAndHashing where

import EasyTest
import Unison.Parsers (unsafeParseTerm)
import qualified Unison.SerializationAndHashing as SAH

testTermString :: String -> Test ()
testTermString termString =
  let term = unsafeParseTerm termString :: SAH.TermV
      roundTrip = SAH.deserializeTermFromBytes . SAH.serializeTerm $ term
  in case roundTrip of
    Left s -> crash s
    Right t -> if t == term then ok else crash (show t)

fortyTwo :: Test ()
fortyTwo = testTermString "42"

fortyTwoNum :: Test ()
fortyTwoNum = testTermString "42 : Number"

hiText :: Test ()
hiText = testTermString "\"hi\" : Text"

lambda :: Test ()
lambda = testTermString "x -> x"

letBinding :: Test ()
letBinding = testTermString "let { x = 42; x + 1 }"

letRec :: Test ()
letRec = testTermString "let rec { x = x + 1; x }"

vec :: Test ()
vec = testTermString "[\"a\", \"b\", \"c\"]"

test :: Test ()
test = scope "SerializationAndHashing" . tests $
  [ scope "fortyTwo" fortyTwo
  , scope "fortyTwoNum" fortyTwoNum
  , scope "hiText" hiText
  , scope "lambda" lambda
  , scope "letBinding" letBinding
  , scope "letRec" letRec
  , scope "vec" vec
  ]
