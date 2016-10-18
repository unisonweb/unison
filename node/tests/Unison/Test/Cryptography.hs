

module Unison.Test.Cryptography where

import qualified Unison.Cryptography as C
import qualified Data.ByteString.Char8 as B
import Unison.Runtime.Cryptography
import Test.Tasty
import Test.Tasty.HUnit

cryptoTest :: IO Assertion
cryptoTest = do
  let crypto = mkCrypto (B.pack "dummypublickey")
      cleartext = (B.pack "cleartext")
  symkey <- C.randomBytes crypto 32
  cyphertext <- C.encrypt crypto symkey [cleartext]
  let decypheredtext = C.decrypt crypto symkey cyphertext
  case decypheredtext of
    Left _ -> fail "Roundtrip encryption failure."
    Right d -> return $ assertEqual "Original cleartext is not equal to decrypted message." cleartext d

ioTests :: IO TestTree
ioTests = fmap (testCase "Cryptography roundtrip test.") cryptoTest

