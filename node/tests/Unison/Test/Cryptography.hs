

module Unison.Test.Cryptography where

import qualified Unison.Cryptography as C
import qualified Data.ByteString.Char8 as B
import Unison.Runtime.Cryptography
import Crypto.Noise.DH
import Crypto.Noise.DH.Curve25519
import Data.Either
import Test.Tasty
import Test.Tasty.HUnit

cryptoTest :: IO Assertion
cryptoTest = do
  keyPair <- dhGenKey :: IO (KeyPair Curve25519)
  let crypto = mkCrypto keyPair
      cleartext = (B.pack "cleartext")
  bytes <- C.randomBytes crypto 32
  let symkey = case symmetricKey bytes of
        Nothing -> error "Error creating symmetric encryption key."
        Just k -> k
  ciphertext <- C.encrypt crypto symkey [cleartext]
  let decipheredtext = C.decrypt crypto symkey ciphertext
  case decipheredtext of
    Left m -> fail ("Roundtrip encryption failure: " ++ m)
    Right d -> return $ assertEqual "Original cleartext is not equal to decrypted message." cleartext d

ioTests :: IO TestTree
ioTests = fmap (testCase "Cryptography roundtrip test.") cryptoTest

