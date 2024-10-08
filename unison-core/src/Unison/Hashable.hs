module Unison.Hashable
  ( accumulate',
    Accumulate (..),
    Token (..),
  )
where

import Crypto.Hash qualified as CH
import Data.ByteArray qualified as BA
import Data.ByteString qualified as B
import Data.ByteString.Builder (doubleBE, int64BE, toLazyByteString, word64BE)
import Data.ByteString.Lazy qualified as BL
import Unison.Hash (Hash)
import Unison.Hash qualified as Hash
import Unison.Prelude

data Token h
  = Tag !Word8
  | Bytes !ByteString
  | Int !Int64
  | Text !Text
  | Double !Double
  | Hashed !h
  | Nat !Word64
  deriving stock (Show)

class Accumulate h where
  accumulate :: [Token h] -> h
  fromBytes :: ByteString -> h
  toBytes :: h -> ByteString

accumulate' :: (Accumulate h, Hashable t) => t -> h
accumulate' = accumulate . tokens

-- | NOTE: This typeclass is distinct from 'Unison.Hashing.V2.Hashable', which is the
-- content-based hashish class used for Unison types & terms.
--
-- This class however, is meant only to be used as a utility when hash-based identities are
-- useful in algorithms, the runtime, etc.
-- Consider carefully which class you want in each use-case.
class Hashable t where
  tokens :: (Accumulate h) => t -> [Token h]

instance Hashable ByteString where
  tokens bs = [Bytes bs]

instance Accumulate Hash where
  accumulate = fromBytes . BA.convert . CH.hashFinalize . go CH.hashInit
    where
      go :: CH.Context CH.SHA3_512 -> [Token Hash] -> CH.Context CH.SHA3_512
      go acc tokens = CH.hashUpdates acc (tokens >>= toBS)
      toBS (Tag b) = [B.singleton b]
      toBS (Bytes bs) = [encodeLength $ B.length bs, bs]
      toBS (Int i) = BL.toChunks . toLazyByteString . int64BE $ i
      toBS (Nat i) = BL.toChunks . toLazyByteString . word64BE $ i
      toBS (Double d) = BL.toChunks . toLazyByteString . doubleBE $ d
      toBS (Text txt) =
        let tbytes = encodeUtf8 txt
         in [encodeLength (B.length tbytes), tbytes]
      toBS (Hashed h) = [Hash.toByteString h]
      encodeLength :: (Integral n) => n -> B.ByteString
      encodeLength = BL.toStrict . toLazyByteString . word64BE . fromIntegral
  fromBytes = Hash.fromByteString
  toBytes = Hash.toByteString
