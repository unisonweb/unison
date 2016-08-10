{-# Language OverloadedStrings #-}

module Unison.Cryptography where

import Control.Concurrent.STM (STM,atomically)
import Control.Concurrent.STM.TVar
import Control.Monad
import Data.ByteString (ByteString)
import Data.Functor
import Data.List
import Data.Maybe
import System.Random (randomIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import qualified Data.Digest.Murmur64 as Murmur

type DoneHandshake = Bool
type Ciphertext = ByteString

-- | The noop cryptography object. Does no actual encryption or signing,
-- and hashing function is not cryptographically secure! Useful for testing / debugging.
noop :: ByteString -> Cryptography ByteString () () () ByteString ByteString ByteString
noop key = Cryptography key gen hash sign verify randomBytes encryptAsymmetric decryptAsymmetric encrypt decrypt pipeInitiator pipeResponder where
  gen = pure ((), ())
  hash = finish . foldl' (\acc bs -> Murmur.hash64Add bs acc) (Murmur.hash64 ())
  sign _ = "not-a-real-signature" :: ByteString
  verify _ _ _ = True
  randomBytes n = B.pack <$> replicateM n randomIO
  encryptAsymmetric _ cleartext = pure cleartext
  decryptAsymmetric ciphertext = Right ciphertext
  encrypt _ bs = pure $ B.concat bs
  decrypt _ bs = Right bs
  -- include the key along with each packet
  pipeInitiator _ = do
    done <- atomically $ newTVar False
    let keyfill = key `mappend` B.replicate (32 - B.length key) 0
    pure (readTVar done, \bs -> writeTVar done True $> (keyfill `mappend` bs), pure)
  -- strip out the key the begins each packet
  pipeResponder = do
    peerKey <- atomically $ newTVar Nothing
    pure (isJust <$> readTVar peerKey, readTVar peerKey, pure, read peerKey)
    where
    read peerKey bytes = do
      let k = B.take 32 bytes
      writeTVar peerKey (Just k)
      pure $ B.drop 32 bytes

  finish h64 = (LB.toStrict . Builder.toLazyByteString . Builder.word64LE . Murmur.asWord64) h64

data Cryptography key symmetricKey signKey signKeyPrivate signature hash cleartext =
  Cryptography
    -- public key
    { publicKey :: key
    -- generate a keypair used for signing
    , generateSignKey :: IO (signKey, signKeyPrivate)
    -- hash some bytes
    , hash :: [ByteString] -> hash
    -- sign some bytes
    , sign :: ByteString -> signature
    -- verify that the signature is a signature by the given `signKey` of the given bytes
    , verify :: signKey -> signature -> ByteString -> Bool
    -- cryptographically random bytes
    , randomBytes :: Int -> IO ByteString
    -- encrypt a message using the given public key, producing public-key ciphertext
    , encryptAsymmetric :: key -> cleartext -> IO Ciphertext
    -- decrypt public key ciphertext, using the private key associated with `publicKey`
    , decryptAsymmetric :: Ciphertext -> Either String cleartext
    -- symmetrically encrypt
    , encrypt :: symmetricKey -> [cleartext] -> IO Ciphertext
    -- symmetrically decrypt
    , decrypt :: symmetricKey -> ByteString -> Either String cleartext
    -- Initiate a secure pipe. Does not perform transport. Returns a function used to
    -- encrypt cleartext for sending to the other party, and a receiving function
    -- used to decrypt messages received back from the other party.
    , pipeInitiator :: key -> IO ( STM DoneHandshake
                                 , cleartext -> STM ByteString
                                 , ByteString -> STM cleartext)
    -- Respond to a secure pipe initiated by another party. Does not perform transport.
    -- Returns a function used to encrypt cleartext for sending to the other party, and a
    -- receiving function used to decrypt messages received back from the other party. Also
    -- receives the other party's public key, after handshaking completes.
    , pipeResponder :: IO ( STM DoneHandshake, STM (Maybe key)
                          , cleartext -> STM ByteString
                          , ByteString -> STM cleartext)
    }

