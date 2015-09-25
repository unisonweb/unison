{-# LANGUAGE TemplateHaskell #-}

module Unison.Hash (Hash, hashBytes, base64, fromBase64, fromBytes) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.ByteString.Base64.URL as Base64

-- | Hash which uniquely identifies a Unison type or term
newtype Hash = Hash ByteString deriving (Eq,Ord)

instance Show Hash where
  show h = "#" ++ (take 5 . drop 1 $ show (base64 h))

-- | Return the base64 encoding of this 'Hash'
base64 :: Hash -> Text
base64 (Hash h) = decodeUtf8 (Base64.encode h)

-- | Produce a 'Hash' from a base64-encoded version of its binary representation
fromBase64 :: Text -> Hash
fromBase64 = Hash . Base64.decodeLenient . encodeUtf8

fromBytes :: ByteString -> Hash
fromBytes = Hash

hashBytes :: Hash -> ByteString
hashBytes (Hash h) = h

instance FromJSON Hash where
  parseJSON j = fromBase64 <$> parseJSON j

instance ToJSON Hash where
  toJSON h = toJSON (base64 h)

