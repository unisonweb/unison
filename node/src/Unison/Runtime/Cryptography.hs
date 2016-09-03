module Unison.Runtime.Cryptography where

import Unison.Cryptography
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Either
import qualified Data.ByteArray as BA
import qualified Crypto.Random as R
import qualified Crypto.Cipher.ChaCha as ChaCha

nonce' :: IO ByteString
nonce' = randomBytes' 8

numRounds :: Int
numRounds = 20

nonce'' :: ByteString
nonce'' = BS8.pack "\xe1\x04\x7b\xa9\x47\x6b\xf8\xff"

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
  encrypt sk bss = do let cleartext = BA.concat bss
                          chacha = ChaCha.initialize numRounds sk nonce''
                          (cyphertext, _) = ChaCha.combine chacha cleartext
                      return cyphertext
  decrypt sk bs = Right $ let chacha = ChaCha.initialize numRounds sk nonce''
                              ba = BA.pack . BS.unpack $ bs
                              (cleartext, _) = ChaCha.combine chacha ba
                          in cleartext
  pipeInitiator _ = undefined
  pipeResponder = undefined

randomBytes' :: Int -> IO ByteString
randomBytes' n = do drg <- R.getSystemDRG
                    let bts = fst $ R.randomBytesGenerate n drg
                    return bts
