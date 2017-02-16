module Unison.Test.Cryptography where

import EasyTest
import Control.Monad
import Control.Monad.STM
import qualified Unison.Cryptography as C
import Unison.Runtime.Cryptography
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Crypto.Noise.DH
import Crypto.Noise.DH.Curve25519
import Data.Either

testEncryptDecrypt :: Test ()
testEncryptDecrypt = do
  keyPair <- io (dhGenKey :: IO (KeyPair Curve25519))
  let crypto = mkCrypto keyPair
  Just symkey <- symmetricKey <$> io (C.randomBytes crypto 32)
  bigSizes <- listOf 3 (int' 1000 9000)
  cleartexts <- map B.pack <$> listsOf ([0..100] ++ bigSizes) word8
  cleartexts `forM_` \cleartext -> do
    ciphertext <- io (C.encrypt crypto symkey [cleartext])
    let cleartext' = C.decrypt crypto symkey ciphertext
    case cleartext' of
      Left err -> crash err
      Right cleartext' -> expect (cleartext == cleartext')

testPipe :: Test ()
testPipe = do
  ikp <- io (dhGenKey :: IO (KeyPair Curve25519)) -- initiator key pair
  rkp <- io (dhGenKey :: IO (KeyPair Curve25519)) -- responder key pair
  let initiator = mkCrypto ikp :: C.Cryptography (PublicKey Curve25519) SymmetricKey () () () B.ByteString B.ByteString
      responder = mkCrypto rkp :: C.Cryptography (PublicKey Curve25519) SymmetricKey () () () B.ByteString B.ByteString
      rpk = snd rkp -- remote public key
  (doneHandshake, iencrypt, idecrypt) <- io $ C.pipeInitiator initiator rpk
  (_, _, rencrypt, rdecrypt) <- io $ C.pipeResponder responder
  go doneHandshake iencrypt rdecrypt
  where go doneHandshake iencrypt rdecrypt = do
          let plaintext = B8.pack "We have done the impossible, and that makes us mighty."
          ready <- io $ atomically doneHandshake
          case ready of
            True -> do
              ciphertext <- io $ atomically $ iencrypt plaintext
              plaintext' <- io $ atomically $ rdecrypt ciphertext
              expect (plaintext == plaintext')
            False -> crash "Temp fail."

test :: Test ()
test = tests [ scope "encrypt/decrypt roundtrip" testEncryptDecrypt
             , scope "Pipes roundtrip" testPipe
             ]
