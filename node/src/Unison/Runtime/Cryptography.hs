{-# LANGUAGE OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}

module Unison.Runtime.Cryptography where

import Unison.Cryptography
import Data.ByteString (ByteString)
import Data.ByteArray as BA
import qualified Crypto.Random as R

-- cryptonite
-- import Crypto.Cipher.Types.Base ()
-- import Crypto.Cipher.AES.Primitive (AES)
import qualified Crypto.Cipher.AES as AES
import Crypto.Cipher.Types
import Crypto.Error

-- Creates a Unison.Cryptography object specialized to use the noise protocol
-- (http://noiseprotocol.org/noise.html).
mkCrypto :: forall cleartext symmetricKey . (ByteArrayAccess cleartext, ByteArray cleartext, ByteArrayAccess symmetricKey, ByteArray symmetricKey) => ByteString -> Cryptography ByteString symmetricKey () () () () cleartext
mkCrypto key = Cryptography key gen hash sign verify randomBytes encryptAsymmetric decryptAsymmetric encrypt decrypt pipeInitiator pipeResponder where
  -- generates an elliptic curve keypair, for use in ECDSA
  gen = undefined
  hash = undefined
  sign _ = undefined
  verify _ _ _ = undefined
  randomBytes = randomBytes'
  encryptAsymmetric _ cleartext = undefined
  decryptAsymmetric ciphertext = undefined

  encrypt :: symmetricKey -> [cleartext] -> IO Ciphertext
  encrypt = encrypt'
      
  decrypt :: symmetricKey -> ByteString -> Either String cleartext
  decrypt = decrypt'

  pipeInitiator _ = undefined
  pipeResponder = undefined

randomBytes' :: Int -> IO ByteString
randomBytes' n = do drg <- R.getSystemDRG
                    let bts = fst $ R.randomBytesGenerate n drg
                    return bts

authTagBitLength :: Int
authTagBitLength = 128

blockLength :: Int
blockLength = 128

encrypt' :: forall cleartext symmetricKey .
            ( ByteArrayAccess cleartext
            , ByteArray cleartext
            , ByteArrayAccess symmetricKey
            , ByteArray symmetricKey)
         => symmetricKey
         -> [cleartext]
         -> IO Ciphertext
encrypt' k cts = do
  iv <- randomBytes' 16
  let clrtext = BA.concat cts :: cleartext
      cipher = throwCryptoError $ cipherInit k :: AES.AES128
      aead = throwCryptoError $ aeadInit AEAD_GCM cipher iv
      ad = "" :: ByteString -- associated data
      ((AuthTag auth), out) = aeadSimpleEncrypt aead ad clrtext authTagBitLength
      ciphertext = BA.concat [(BA.convert auth), iv, (BA.convert out)]
  return ciphertext

decrypt' :: forall cleartext symmetricKey .
            ( ByteArrayAccess symmetricKey
            , ByteArray symmetricKey
            , ByteArray cleartext)
         => symmetricKey
         -> ByteString
         -> Either String cleartext
decrypt' k ciphertext = let (auth, ct') = BA.splitAt authTagBitLength ciphertext
                            (iv, ct'') = BA.splitAt blockLength ct'
                            cipher = throwCryptoError $ cipherInit (k :: symmetricKey) :: AES.AES128
                            aead = throwCryptoError $ aeadInit AEAD_GCM cipher iv
                            ad = "" :: ByteString -- associated data
                            maybeCleartext = aeadSimpleDecrypt aead ad ct'' (AuthTag (BA.convert auth))
                        in
                          case maybeCleartext of
                          Just pt -> Right $ BA.convert pt
                          Nothing -> Left "Error when attempting to decrypt ciphertext."
