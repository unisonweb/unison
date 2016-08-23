module Unison.Runtime.Cryptography where

import Unison.Cryptography
import Data.ByteString (ByteString)
import qualified Crypto.Random as R

-- Creates a Unison.Cryptography object specialized to use the noise protocol
-- (http://noiseprotocol.org/noise.html).
noise :: ByteString -> Cryptography ByteString () () () () () ()
noise key = Cryptography key gen hash sign verify randomBytes encryptAsymmetric decryptAsymmetric encrypt decrypt pipeInitiator pipeResponder where
  -- generates an elliptic curve keypair, for use in ECDSA
  gen = undefined
  hash = undefined
  sign _ = undefined
  verify _ _ _ = undefined
  randomBytes n = do
    drg <- R.getSystemDRG
    let bts = fst $ R.randomBytesGenerate n drg
    return bts
  encryptAsymmetric _ cleartext = undefined
  decryptAsymmetric ciphertext = undefined
  encrypt _ bs = undefined
  decrypt _ bs = undefined
  pipeInitiator _ = undefined
  pipeResponder = undefined
