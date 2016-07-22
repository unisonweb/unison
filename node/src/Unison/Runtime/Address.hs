{-# Language DeriveGeneric #-}
module Unison.Runtime.Address where

import Data.ByteString (ByteString)
import Data.Bytes.Serial
import Data.SafeCopy
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import qualified Data.Digest.Murmur64 as Murmur

newtype Address = Address ByteString deriving (Eq,Ord,Show,Generic)

makeAddress :: ByteString -> Address
makeAddress = fromBytes . LB.toStrict
    . Builder.toLazyByteString . Builder.word64LE . Murmur.asWord64 . Murmur.hash64

instance SafeCopy Address where
  putCopy (Address a) = contain $ safePut a
  getCopy = contain $ Address <$> safeGet

fromBytes :: ByteString -> Address
fromBytes = Address

toBytes :: Address -> ByteString
toBytes (Address bs) = bs

toBase64 :: Address -> Text
toBase64 (Address bs) = decodeUtf8 (Base64.encode bs)

fromBase64 :: Text -> Address
fromBase64 = Address . Base64.decodeLenient . encodeUtf8

instance Serial Address
