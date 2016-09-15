module Unison.Runtime.Cryptography where

import Unison.Cryptography
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either
import qualified Data.ByteArray as BA
import qualified Crypto.Random as R
import qualified Crypto.Cipher.ChaCha as ChaCha

numRounds :: Int
numRounds = 20

nonceSize :: Int
nonceSize = 12

-- Creates a Unison.Cryptography object specialized to use the noise protocol
-- (http://noiseprotocol.org/noise.html).
noise :: (BA.ByteArray cleartext, BA.ByteArray symmetricKey) => ByteString -> Cryptography ByteString symmetricKey () () () () cleartext
noise key = Cryptography key gen hash sign verify randomBytes encryptAsymmetric decryptAsymmetric encrypt decrypt pipeInitiator pipeResponder where
  -- generates an elliptic curve keypair, for use in ECDSA
  gen = undefined
  hash = undefined
  sign _ = undefined
  verify _ _ _ = undefined
  randomBytes = randomBytes'
  encryptAsymmetric _ cleartext = undefined
  decryptAsymmetric ciphertext = undefined
  encrypt sk bss = do nonce <- randomBytes' nonceSize
                      let cleartext = BA.concat bss
                          chacha = ChaCha.initialize numRounds sk nonce
                          (ciphertext, _) = ChaCha.combine chacha cleartext
                          fct = BS.append nonce ciphertext -- add the nonce to the front of the ciphertext to produce the final ciphertext
                      return fct
  decrypt sk bs = let (nonce, ciphertext) = BS.splitAt nonceSize bs -- nonce is stored at the beginning of the ciphertext
                      chacha = ChaCha.initialize numRounds sk nonce
                      (cleartxt, _) = ChaCha.combine chacha ciphertext
                  in Right $ (BA.pack . BS.unpack) cleartxt
  pipeInitiator _ = undefined
  pipeResponder = undefined

randomBytes' :: Int -> IO ByteString
randomBytes' n = do drg <- R.getSystemDRG
                    let bts = fst $ R.randomBytesGenerate n drg
                    return bts
