{-# LANGUAGE DeriveGeneric #-}

module Unison.Hash (Hash, toBytes, base58, fromBase58, fromBytes, unsafeFromBase58) where

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import qualified Data.ByteString.Base58 as Base58
import qualified Data.Text as Text

-- | Hash which uniquely identifies a Unison type or term
newtype Hash = Hash ByteString deriving (Eq,Ord,Generic)

instance Show Hash where
  show h = take 8 $ show (base58 h)

-- | Return the base58 encoding of this 'Hash'
base58 :: Hash -> Text
base58 (Hash h) = decodeUtf8 (Base58.encodeBase58 Base58.bitcoinAlphabet h)

-- | Produce a 'Hash' from a base58-encoded version of its binary representation
fromBase58 :: Text -> Maybe Hash
fromBase58 txt = Hash <$> Base58.decodeBase58 Base58.bitcoinAlphabet (encodeUtf8 txt)

unsafeFromBase58 :: Text -> Hash
unsafeFromBase58 txt = case fromBase58 txt of
  Just h -> h
  Nothing -> error $ "invalid base58: " ++ Text.unpack txt

fromBytes :: ByteString -> Hash
fromBytes = Hash

toBytes :: Hash -> ByteString
toBytes (Hash h) = h

instance FromJSON Hash where
  parseJSON j = do
    txt <- parseJSON j
    Just b58 <- pure (fromBase58 txt)
    pure b58

instance ToJSON Hash where
  toJSON h = toJSON (base58 h)
