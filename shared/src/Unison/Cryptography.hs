{-# Language OverloadedStrings #-}

module Unison.Cryptography where

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Builder as Builder
import Data.List
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import qualified Data.Digest.Murmur64 as Murmur

type DoneHandshake = Bool
type Ciphertext = ByteString

-- | The noop cryptography object. Does no actual encryption or signing,
-- and hashing function is not cryptographically secure! Useful for testing / debugging.
noop :: Cryptography () () () ByteString ByteString ByteString
noop = Cryptography () () hash sign verify randomBytes encryptAsymmetric decryptAsymmetric encrypt decrypt pipeInitiator pipeResponder where
  hash = finish . foldl' (\acc bs -> Murmur.hash64Add bs acc) (Murmur.hash64 ())
  sign _ = "not-a-real-signature" :: ByteString
  verify _ _ _ = True
  randomBytes n = pure $ ByteString.replicate n 4 -- see: https://xkcd.com/221/
  encryptAsymmetric _ cleartext = pure cleartext
  decryptAsymmetric ciphertext = Right ciphertext
  encrypt _ bs = pure $ ByteString.concat bs
  decrypt _ bs = Right bs
  pipeInitiator _ = pure (pure True, pure, pure)
  pipeResponder = pure (pure True, pure (Just ()), pure, pure)
  finish h64 = (LB.toStrict . Builder.toLazyByteString . Builder.word64LE . Murmur.asWord64) h64

data Cryptography key symmetricKey signKey signature hash cleartext =
  Cryptography
    -- public key
    { publicKey :: key
    -- public key, used for signing (may not be the same as `publicKey`)
    , publicSigningKey :: signKey
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
    -- Initiate a secure pipe. Does not perform transport. Returns a function used to encrypt
    -- cleartext for sending to the other party, and a receiving function used to decrypt messages
    -- received back from the other party.
    , pipeInitiator :: key -> IO (IO DoneHandshake, cleartext -> IO ByteString, ByteString -> IO cleartext)
    -- Respond to a secure pipe initiated by another party. Does not perform transport.
    -- Returns a function used to encrypt cleartext for sending to the other party, and a
    -- receiving function used to decrypt messages received back from the other party. Also
    -- receives the other party's public key, after handshaking completes.
    , pipeResponder :: IO (IO DoneHandshake, IO (Maybe key), cleartext -> IO ByteString, ByteString -> IO cleartext) }

