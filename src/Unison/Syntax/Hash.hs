{-# LANGUAGE TemplateHaskell #-}

module Unison.Syntax.Hash (
  Hash, Digest,
  append, base64, fromBase64, byte, bytes, finalize, hashBytes,
  lazyBytes, zero, one, two, three) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Base64 as Base64
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Crypto.Hash.SHA3 as H

-- | Hash which uniquely identifies a Unison type or term
newtype Hash = Hash B.ByteString deriving (Eq,Ord)

instance Show Hash where
  show h = "#" ++ (take 5 . drop 1 $ show (base64 h))

-- | Buffer type for building up hash values
newtype Digest = Digest (H.Ctx -> H.Ctx)

append :: Digest -> Digest -> Digest
append (Digest a) (Digest b) = Digest (b . a)

-- | Return the base64 encoding of this 'Hash'
base64 :: Hash -> T.Text
base64 (Hash h) = decodeUtf8 (Base64.encode h)

-- | Produce a 'Hash' from a base64-encoded version of its binary representation
fromBase64 :: T.Text -> Hash
fromBase64 = Hash . Base64.decodeLenient . encodeUtf8

hashBytes :: Hash -> B.ByteString
hashBytes (Hash h) = h

finalize :: Digest -> Hash
finalize (Digest f) =
  Hash . H.finalize . f . H.init $ 256

bytes :: B.ByteString -> Digest
bytes bs = Digest (\ctx -> H.update ctx bs)

lazyBytes :: LB.ByteString -> Digest
lazyBytes bs = Digest (\ctx -> H.updates ctx (LB.toChunks bs))

byte :: Word8 -> Digest
byte b = bytes (B.singleton b)

zero :: Digest
zero = byte 0

one :: Digest
one = byte 1

two :: Digest
two = byte 2

three :: Digest
three = byte 3

instance FromJSON Hash where
  parseJSON j = fromBase64 <$> parseJSON j

instance ToJSON Hash where
  toJSON h = toJSON (base64 h)
