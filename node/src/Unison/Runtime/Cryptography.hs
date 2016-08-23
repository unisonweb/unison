module Unison.Runtime.Cryptography where

import Unison.Cryptography
import Data.ByteString (ByteString)

-- Creates a Unison.Cryptography object specialized to use the noise protocol
-- (http://noiseprotocol.org/noise.html).
noise :: ByteString -> Cryptography ByteString () () () () () ()
noise key = Cryptography key gen hash sign verify randomBytes encryptAsymmetric decryptAsymmetric encrypt decrypt pipeInitiator pipeResponder where
  gen = undefined
  hash = undefined
  sign _ = undefined
  verify _ _ _ = undefined
  randomBytes n = undefined
  encryptAsymmetric _ cleartext = undefined
  decryptAsymmetric ciphertext = undefined
  encrypt _ bs = undefined
  decrypt _ bs = undefined
  pipeInitiator _ = undefined
  pipeResponder = undefined
