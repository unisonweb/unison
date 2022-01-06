module Unison.Hashing.V2.BuildHashable where

import qualified Crypto.Hash as CH
import qualified Data.ByteArray as BA
import qualified Data.ByteString as B
import Data.ByteString.Builder (doubleBE, int64BE, toLazyByteString, word64BE)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Data.Set as Set
import U.Util.Hash (Hash)
import qualified U.Util.Hash as Hash
import Unison.Prelude
import Unison.Util.Relation (Relation)
import qualified Unison.Util.Relation as Relation
import Unison.Util.Relation3 (Relation3)
import qualified Unison.Util.Relation3 as Relation3
import Unison.Util.Relation4 (Relation4)
import qualified Unison.Util.Relation4 as Relation4

data Token h
  = Tag !Word8
  | Bytes !ByteString
  | Int !Int64
  | Text !Text
  | Double !Double
  | Hashed !h
  | Nat !Word64

class Accumulate h where
  accumulate :: [Token h] -> h
  fromBytes :: ByteString -> h
  toBytes :: h -> ByteString

accumulateToken :: (Accumulate h, Hashable t) => t -> Token h
accumulateToken = Hashed . accumulate'

hash, accumulate' :: (Accumulate h, Hashable t) => t -> h
hash = accumulate'
accumulate' = accumulate . (hashVersion :) . tokens
  where
    hashVersion = Tag 2

class Hashable t where
  tokens :: Accumulate h => t -> [Token h]

instance Hashable a => Hashable [a] where
  tokens = map accumulateToken

instance (Hashable a, Hashable b) => Hashable (a, b) where
  tokens (a, b) = [accumulateToken a, accumulateToken b]

instance (Hashable a) => Hashable (Set.Set a) where
  tokens = tokens . Set.toList

instance (Hashable k, Hashable v) => Hashable (Map.Map k v) where
  tokens = tokens . Map.toList

instance (Hashable a, Hashable b) => Hashable (Relation a b) where
  tokens = tokens . Relation.toList

instance (Hashable d1, Hashable d2, Hashable d3) => Hashable (Relation3 d1 d2 d3) where
  tokens s = [accumulateToken $ Relation3.toNestedList s]

instance (Hashable d1, Hashable d2, Hashable d3, Hashable d4) => Hashable (Relation4 d1 d2 d3 d4) where
  tokens s = [accumulateToken $ Relation4.toNestedList s]

class Hashable1 f where
  -- | Produce a hash for an `f a`, given a hashing function for `a`.
  -- If there is a notion of order-independence in some aspect of a subterm
  -- of `f`, then the first argument (`hashUnordered :: [a] -> ([h], a -> h)`)
  -- should be used to impose an order, and then apply that order in further hashing.
  -- Otherwise the second argument (`hash :: a -> h`) should be used.
  --
  -- Example 1: A simple functor with no unordered components. Hashable1 instance
  --            just uses `hash`:
  --
  --   data T a = One a | Two a a deriving Functor
  --
  --   instance Hashable1 T where
  --     hash1 _ hash t = case t of
  --       One a -> accumulate [Tag 0, Hashed (hash a)]
  --       Two a a2 -> accumulate [Tag 1, Hashed (hash a), Hashed (hash a2)]
  --
  -- Example 2: A functor with unordered components. For hashing, we need to
  --            pick a canonical ordering of the unordered components, so we
  --            use `hashUnordered`:
  --
  --   data U a = U { unordered :: [a], uno :: a, dos :: a } deriving Functor
  --
  --   instance Hashable1 U where
  --     hash1 hashUnordered _ (U unordered uno dos) =
  --       let (hs, hash) = hashUnordered unordered
  --       in accumulate $ map Hashed hs ++ [Hashed (hash uno), Hashed (hash dos)]
  hash1 :: (Ord h, Accumulate h) => ([a] -> ([h], a -> h)) -> (a -> h) -> f a -> h

instance Hashable () where
  tokens _ = []

instance Hashable Double where
  tokens d = [Double d]

instance Hashable Text where
  tokens s = [Text s]

instance Hashable Char where
  tokens c = [Nat $ fromIntegral $ fromEnum c]

instance Hashable ByteString where
  tokens bs = [Bytes bs]

instance Hashable Word64 where
  tokens w = [Nat w]

instance Hashable Int64 where
  tokens w = [Int w]

instance Hashable Bool where
  tokens b = [Tag . fromIntegral $ fromEnum b]

instance Hashable Hash where
  tokens h = [Bytes (Hash.toByteString h)]

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
      encodeLength :: Integral n => n -> B.ByteString
      encodeLength = BL.toStrict . toLazyByteString . word64BE . fromIntegral
  fromBytes = Hash.fromByteString
  toBytes = Hash.toByteString
