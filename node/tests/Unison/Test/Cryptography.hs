

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
  symkey <- C.randomBytes crypto 16
  ciphertext <- C.encrypt crypto symkey [cleartext]
  let decipheredtext = C.decrypt crypto symkey ciphertext
  case decipheredtext of
    Left m -> fail ("Roundtrip encryption failure: " ++ m)
    Right d -> return $ assertEqual "Original cleartext is not equal to decrypted message." cleartext d

ioTests :: IO TestTree
ioTests = fmap (testCase "Cryptography roundtrip test.") cryptoTest

