{-# LANGUAGE OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language GeneralizedNewtypeDeriving #-}

module Unison.Runtime.Cryptography
       ( symmetricKey
       , SymmetricKey
       , mkCrypto
       ) where

import Unison.Cryptography
import Prelude hiding (writeFile, readFile)
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString, readFile)
import qualified Data.ByteString.Base64 as B64 (decodeLenient)
import Data.ByteArray as BA
import qualified Crypto.Random as R
import Control.Concurrent.STM (STM,atomically)
import Control.Concurrent.STM.TVar
import Control.Lens ((.~), (&))

-- cryptonite
import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Hash as H
import Crypto.Hash.Algorithms (SHA256)
import Crypto.Cipher.Types
import Crypto.Error

-- cacophony
import Crypto.Noise
import Crypto.Noise.HandshakePatterns (noiseIK)
import Crypto.Noise.DH
import Crypto.Noise.DH.Curve25519
import Crypto.Noise.Cipher.AESGCM
import qualified Crypto.Noise.Hash.SHA256 as CacHash

newtype SymmetricKey = AES256 ByteString deriving (Ord, Eq, Monoid, ByteArrayAccess, ByteArray)

symmetricKey :: ByteString -> Maybe SymmetricKey
symmetricKey bs | BA.length bs == 32 = (Just . AES256) bs
                | otherwise = Nothing

-- Creates a Unison.Cryptography object specialized to use the noise protocol
-- (http://noiseprotocol.org/noise.html).
mkCrypto :: forall cleartext . (ByteArrayAccess cleartext, ByteArray cleartext) => KeyPair Curve25519 -> Cryptography (PublicKey Curve25519) SymmetricKey () () () ByteString cleartext
mkCrypto staticKeyPair@(privateKey, publicKey') = Cryptography publicKey gen hash sign verify randomBytes encryptAsymmetric decryptAsymmetric encrypt decrypt pipeInitiator pipeResponder where
  publicKey = publicKey'

  -- generates an elliptic curve keypair, for use in ECDSA
  gen = undefined

  hash :: [ByteString] -> ByteString
  hash bss = BA.convert (H.hash bs :: H.Digest SHA256)
    where bs = BA.concat bss :: ByteString

  sign _ = undefined
  verify _ _ _ = undefined
  randomBytes = randomBytes'
  encryptAsymmetric _ cleartext = undefined
  decryptAsymmetric ciphertext = undefined

  encrypt :: SymmetricKey -> [cleartext] -> IO Ciphertext
  encrypt = encrypt'
      
  decrypt :: SymmetricKey -> ByteString -> Either String cleartext
  decrypt = decrypt'

  pipeInitiator = pipeInitiator' staticKeyPair

  pipeResponder = pipeResponder' staticKeyPair

randomBytes' :: Int -> IO ByteString
randomBytes' n = fst . R.randomBytesGenerate n <$> R.getSystemDRG

-- The number of bits in the Initialization Vector (IV). This should be equal to
-- the block size, which is 128 in this implementation.
ivBitLength :: Int
ivBitLength = 128

-- The number of bits in the authorization tag. In general this value can be one
-- of 128, 120, 112, 104, or 96. The larger, the better.
authTagBitLength :: Int
authTagBitLength = 128

encrypt' :: forall cleartext .
            ( ByteArrayAccess cleartext
            , ByteArray cleartext
            )
         => SymmetricKey
         -> [cleartext]
         -> IO Ciphertext
encrypt' k cts = go <$> randomBytes' (ivBitLength `div` 8)
  where go iv =
          let clrtext = BA.concat cts :: cleartext
              cipher = throwCryptoError $ cipherInit k :: AES.AES256
              aead = throwCryptoError $ aeadInit AEAD_GCM cipher iv
              ad = "" :: ByteString -- associated data
              ((AuthTag auth), out) = aeadSimpleEncrypt aead ad clrtext authTagBitLength
          in
            BA.concat [(BA.convert auth), iv, (BA.convert out)]

decrypt' :: (ByteArray cleartext)
         => SymmetricKey
         -> ByteString
         -> Either String cleartext
decrypt' k ciphertext =
   let (auth, ct') = BA.splitAt (authTagBitLength `div` 8) ciphertext
       (iv, ct'') = BA.splitAt (ivBitLength `div` 8) ct'
       cipher = throwCryptoError $ cipherInit k :: AES.AES256
       aead = throwCryptoError $ aeadInit AEAD_GCM cipher iv
       ad = "" :: ByteString -- associated data
       maybeCleartext = aeadSimpleDecrypt aead ad ct'' (AuthTag (BA.convert auth))
   in
     case maybeCleartext of
       Just pt -> Right $ BA.convert pt
       Nothing -> Left "Error when attempting to decrypt ciphertext."

pipeInitiator' :: forall cleartext .
                  ( ByteArrayAccess cleartext
                  , ByteArray cleartext
                  )
               => KeyPair Curve25519
               -> PublicKey Curve25519
               -> IO ( STM DoneHandshake
                     , cleartext -> STM Ciphertext
                     , Ciphertext -> STM cleartext
                     )
pipeInitiator' staticKeyPair remotePublicKey = do
--  let keyPairFile = "/path/to/keyfile"
--  staticKeyPair <- readKeyPair keyPairFile :: IO (KeyPair Curve25519)
  ephemeralKeyPair <- dhGenKey :: IO (KeyPair Curve25519)
  let dho = defaultHandshakeOpts noiseIK InitiatorRole
      ho = dho & hoRemoteStatic .~ Just remotePublicKey
               & hoLocalEphemeral .~ Just ephemeralKeyPair
               & hoLocalStatic .~ Just staticKeyPair
      ns = noiseState ho :: NoiseState AESGCM Curve25519 CacHash.SHA256
  ns' <- atomically $ newTVar ns
  let f :: cleartext -> STM ByteString
      f ct = do
        ns <- readTVar ns'
        let msg = BA.convert ct
            (ciphertext, ns'') = either (error . show) id $ writeMessage ns msg
        writeTVar ns' ns''
        return ciphertext
      g :: Ciphertext -> STM cleartext
      g ct = do
        ns <- readTVar ns'
        let (msg, ns'') = either (error . show) id $ readMessage ns ct
            msg' = BA.convert msg
        writeTVar ns' ns''
        return msg'
  return (handshakeComplete <$> readTVar ns', f, g)

pipeResponder' :: forall cleartext .
                  ( ByteArrayAccess cleartext
                  , ByteArray cleartext
                  )
               => KeyPair Curve25519
               -> IO ( STM DoneHandshake, STM (Maybe (PublicKey Curve25519))
                     , cleartext -> STM Ciphertext
                     , Ciphertext -> STM cleartext)
pipeResponder' staticKeyPair = do
--  let keyPairFile = "/path/to/keyfile"
--  staticKeyPair <- readKeyPair keyPairFile :: IO (KeyPair Curve25519)
  ephemeralKeyPair <- dhGenKey :: IO (KeyPair Curve25519)
  let dho = defaultHandshakeOpts noiseIK ResponderRole
      ho = dho & hoLocalStatic .~ Just staticKeyPair
               & hoLocalEphemeral .~ Just ephemeralKeyPair
      ns = noiseState ho :: NoiseState AESGCM Curve25519 CacHash.SHA256
  ns' <- atomically $ newTVar ns
  sendersPubKey <- atomically $ newTVar Nothing
  let f :: cleartext -> STM ByteString
      f ct = do
        ns <- readTVar ns'
        let msg = BA.convert ct
            (ciphertext, ns'') = either (error . show) id $ writeMessage ns msg
        writeTVar ns' ns''
        return ciphertext
      g :: Ciphertext -> STM cleartext
      g ct = do
        ns <- readTVar ns'
        let (msg, ns'') = either (error . show) id $ readMessage ns ct
            msg' = BA.convert msg
        writeTVar ns' ns''
        return msg'
  pure (handshakeComplete <$> readTVar ns', readTVar sendersPubKey, f, g)

readKeyPair :: DH d => FilePath -> IO (KeyPair d)
readKeyPair f = fromMaybe (error $ "error importing key from file: " ++ f) . dhBytesToPair . BA.convert <$> readFile f

mkPublicKey :: DH d => ByteString -> PublicKey d
mkPublicKey = (fromMaybe (error "Error converting public key.") . dhBytesToPub . BA.convert . B64.decodeLenient)
