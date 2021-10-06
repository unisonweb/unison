{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module U.Util.Hash where

-- (Hash, toBytes, base32Hex, base32Hexs, fromBase32Hex, fromBytes, unsafeFromBase32Hex, showBase32Hex, validBase32HexChars) where

-- import Unison.Prelude

import Data.ByteString (ByteString)
import qualified Data.ByteString.Short as B.Short
import GHC.Generics (Generic)
import Data.ByteString.Short (fromShort, ShortByteString)
import qualified U.Util.Base32Hex as Base32Hex
import U.Util.Base32Hex (Base32Hex)

-- | Hash which uniquely identifies a Unison type or term
newtype Hash = Hash {toShort :: ShortByteString} deriving (Eq, Ord, Generic)

toBase32Hex :: Hash -> Base32Hex
toBase32Hex = Base32Hex.fromByteString . toBytes

fromBase32Hex :: Base32Hex -> Hash
fromBase32Hex = Hash . B.Short.toShort . Base32Hex.toByteString

toBytes :: Hash -> ByteString
toBytes = fromShort . toShort

-- instance H.Hashable Hash where
--   tokens h = [H.Bytes (toBytes h)]

-- instance H.Accumulate Hash where
--   accumulate = fromBytes . BA.convert . CH.hashFinalize . go CH.hashInit
--     where
--       go :: CH.Context CH.SHA3_512 -> [H.Token Hash] -> CH.Context CH.SHA3_512
--       go acc tokens = CH.hashUpdates acc (tokens >>= toBS)
--       toBS (H.Tag b) = [B.singleton b]
--       toBS (H.Bytes bs) = [encodeLength $ B.length bs, bs]
--       toBS (H.Int i) = BL.toChunks . toLazyByteString . int64BE $ i
--       toBS (H.Nat i) = BL.toChunks . toLazyByteString . word64BE $ i
--       toBS (H.Double d) = BL.toChunks . toLazyByteString . doubleBE $ d
--       toBS (H.Text txt) =
--         let tbytes = encodeUtf8 txt
--          in [encodeLength (B.length tbytes), tbytes]
--       toBS (H.Hashed h) = [toBytes h]
--       encodeLength :: Integral n => n -> B.ByteString
--       encodeLength = BL.toStrict . toLazyByteString . word64BE . fromIntegral
--   fromBytes = U.Util.Hash.fromBytes
--   toBytes = U.Util.Hash.toBytes

fromBytes :: ByteString -> Hash
fromBytes = Hash . B.Short.toShort

instance Show Hash where
  show h = "fromBase32Hex " ++ (show . Base32Hex.toText . toBase32Hex) h