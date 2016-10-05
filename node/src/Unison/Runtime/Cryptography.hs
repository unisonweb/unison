{-# LANGUAGE OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}

module Unison.Runtime.Cryptography where

import Unison.Cryptography
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteArray as BA
import qualified Crypto.Random as R
import qualified Crypto.Cipher.ChaCha as ChaCha
import qualified Crypto.Cipher.ChaChaPoly1305 as AEAD
import qualified Crypto.MAC.Poly1305 as MAC
import Crypto.Error (CryptoFailable(..))

numRounds :: Int
numRounds = 20

nonceSize :: Int
nonceSize = 12

aad :: ByteString
aad = "\x50\x51\x52\x53\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7"

-- Creates a Unison.Cryptography object specialized to use the noise protocol
-- (http://noiseprotocol.org/noise.html).
noise :: forall cleartext symmetricKey . (BA.ByteArrayAccess cleartext, BA.ByteArray cleartext, BA.ByteArrayAccess symmetricKey, BA.ByteArray symmetricKey) => ByteString -> Cryptography ByteString symmetricKey () () () () cleartext
noise key = Cryptography key gen hash sign verify randomBytes encryptAsymmetric decryptAsymmetric encrypt decrypt pipeInitiator pipeResponder where
  -- generates an elliptic curve keypair, for use in ECDSA
  gen = undefined
  hash = undefined
  sign _ = undefined
  verify _ _ _ = undefined
  randomBytes = randomBytes'
  encryptAsymmetric _ cleartext = undefined
  decryptAsymmetric ciphertext = undefined
  -- encrypt sk bss = do nonce <- randomBytes' nonceSize
  --                     let cleartext = BA.concat bssn
  --                         chacha = ChaCha.initialize numRounds sk nonce
  --                         (ciphertext, _) = ChaCha.combine chacha cleartext
  --                         fct = BS.append nonce ciphertext -- add the nonce to the front of the ciphertext to produce the final ciphertext
  --                     return fct
  encrypt :: symmetricKey -> [cleartext] -> IO Ciphertext
  encrypt sk bss = do
    rb <- randomBytes' nonceSize
    let cleartext = BA.concat bss :: cleartext
        encResult = do
          nonce <- AEAD.nonce12 rb
          ini <- AEAD.initialize sk nonce
          let afterAad = AEAD.finalizeAAD (AEAD.appendAAD aad ini)
              (ciphertext, afterEncrypt) = AEAD.encrypt cleartext afterAad
              MAC.Auth outtag = AEAD.finalize afterEncrypt
              payload = BA.append outtag (BA.convert ciphertext)
          return payload
    case encResult of
      CryptoFailed ce -> error ("Enryption error: " ++ show ce)
      CryptoPassed ct -> return $ BA.convert ct
      
  -- decrypt sk bs = let (nonce, ciphertext) = BS.splitAt nonceSize bs -- nonce is stored at the beginning of the ciphertext
  --                     chacha = ChaCha.initialize numRounds sk nonce
  --                     (cleartxt, _) = ChaCha.combine chacha ciphertext
  --                 in Right $ (BA.pack . BS.unpack) cleartxt
  decrypt :: symmetricKey -> ByteString -> Either String cleartext
  decrypt sk bs = undefined

  pipeInitiator _ = undefined
  pipeResponder = undefined

randomBytes' :: Int -> IO ByteString
randomBytes' n = do drg <- R.getSystemDRG
                    let bts = fst $ R.randomBytesGenerate n drg
                    return bts
