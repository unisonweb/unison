{-# LANGUAGE TemplateHaskell #-}

module Unison.Hash (Hash, hashBytes, base64, fromBase64, fromBytes) where

import Control.Applicative
import Data.Aeson
import Data.Bytes.Serial
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

-- | Hash which uniquely identifies a Unison type or term
newtype Hash = Hash B.ByteString deriving (Eq,Ord)

instance Show Hash where
  show h = "#" ++ (take 5 . drop 1 $ show (base64 h))

-- | Return the base64 encoding of this 'Hash'
base64 :: Hash -> T.Text
base64 (Hash h) = decodeUtf8 (Base64.encode h)

-- | Produce a 'Hash' from a base64-encoded version of its binary representation
fromBase64 :: T.Text -> Hash
fromBase64 = Hash . Base64.decodeLenient . encodeUtf8

fromBytes :: B.ByteString -> Hash
fromBytes = Hash

hashBytes :: Hash -> B.ByteString
hashBytes (Hash h) = h

instance FromJSON Hash where
  parseJSON j = fromBase64 <$> parseJSON j

instance ToJSON Hash where
  toJSON h = toJSON (base64 h)

instance Serial Hash where
  serialize (Hash h) = serialize h
  deserialize = Hash <$> deserialize
