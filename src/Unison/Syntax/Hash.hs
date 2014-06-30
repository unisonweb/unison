{-# LANGUAGE TemplateHaskell #-}

module Unison.Syntax.Hash (
  Hash, Digest,
  append, finalize, double, text, bytes, byte, hashBytes,
  zero, one, two, three) where

import qualified Data.ByteString.Base64 as Base64
import Data.Word (Word8)
import Data.Aeson.TH
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Crypto.Hash.SHA3 as H

-- | Hash which uniquely identifies a Unison type or term
newtype Hash = Hash T.Text deriving (Eq,Ord,Show,Read)

-- | Buffer type for building up hash values
newtype Digest = Digest (H.Ctx -> H.Ctx)

append :: Digest -> Digest -> Digest
append (Digest a) (Digest b) = Digest (b . a)

double :: Double -> Digest
double = error "todo: hashDouble"

text :: T.Text -> Digest
text = error "todo: hashText"

hashBytes :: Hash -> B.ByteString
hashBytes (Hash h) = Base64.decodeLenient (encodeUtf8 h)

finalize :: Digest -> Hash
finalize (Digest f) =
  Hash . decodeUtf8 . Base64.encode . H.finalize . f . H.init $ 256

bytes :: B.ByteString -> Digest
bytes bs = Digest (\ctx -> H.update ctx bs)

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

deriveJSON defaultOptions ''Hash
