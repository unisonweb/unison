module Unison.Hashing.V2.Tokenizable
  ( Tokenizable (..),
    Accumulate (..),
    Hashable1 (..),
    Token (..),
    hashTokenizable,
    accumulateToken,
  )
where

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

-- | The version of the current hashing function.
-- This should be incremented every time the hashing function is changed.
--
-- The reasoning is that, if a change to the hashing function changes the hashes for _some_
-- values, it should change it for _all_ values so that we don't have collisions between
-- different hashing function versions. If we don't do this, it's possible for the hashes of
-- simple types (like an Int for example) to keep the same hashes, which would lead to
-- collisions in the `hash` table, since each hash has a different hash version but the same
-- base32 representation.
hashingVersion :: Token h
hashingVersion = Tag 2

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

accumulateToken :: (Accumulate h, Tokenizable t) => t -> Token h
accumulateToken = Hashed . hashTokenizable

-- | Tokenize then accumulate a type into a Hash.
hashTokenizable :: (Tokenizable t, Accumulate h) => t -> h
hashTokenizable = accumulate . tokens

-- | Tokenizable converts a value into a set of hashing tokens which will later be accumulated
-- into a Hash. Be very careful when adding or altering instances of this typeclass, changing
-- the hash of a value is a major breaking change and requires a complete codebase migration.
--
-- If you simply want to provide a convenience instance for a type which wraps some Hashable
-- type, write an instance of 'Hashable' which calls through to the inner instance instead.
--
-- E.g. If I want to be able to hash a @TaggedBranch@ using its Branch0 hashable instance:
--
-- @@
-- data TaggedBranch = TaggedBranch String Branch
--
-- instance Hashable TaggedBranch where
--   hash (TaggedBranch _ b) = hash b
-- @@
class Tokenizable t where
  tokens :: Accumulate h => t -> [Token h]

instance Tokenizable a => Tokenizable [a] where
  tokens = map accumulateToken

instance (Tokenizable a, Tokenizable b) => Tokenizable (a, b) where
  tokens (a, b) = [accumulateToken a, accumulateToken b]

instance (Tokenizable a) => Tokenizable (Set.Set a) where
  tokens = tokens . Set.toList

instance (Tokenizable k, Tokenizable v) => Tokenizable (Map.Map k v) where
  tokens = tokens . Map.toList

instance (Tokenizable a, Tokenizable b) => Tokenizable (Relation a b) where
  tokens = tokens . Relation.toList

instance (Tokenizable d1, Tokenizable d2, Tokenizable d3) => Tokenizable (Relation3 d1 d2 d3) where
  tokens s = [accumulateToken $ Relation3.toNestedList s]

instance (Tokenizable d1, Tokenizable d2, Tokenizable d3, Tokenizable d4) => Tokenizable (Relation4 d1 d2 d3 d4) where
  tokens s = [accumulateToken $ Relation4.toNestedList s]

instance Tokenizable () where
  tokens _ = []

instance Tokenizable Double where
  tokens d = [Double d]

instance Tokenizable Text where
  tokens s = [Text s]

instance Tokenizable Char where
  tokens c = [Nat $ fromIntegral $ fromEnum c]

instance Tokenizable ByteString where
  tokens bs = [Bytes bs]

instance Tokenizable Word64 where
  tokens w = [Nat w]

instance Tokenizable Int64 where
  tokens w = [Int w]

instance Tokenizable Bool where
  tokens b = [Tag . fromIntegral $ fromEnum b]

instance Tokenizable Hash where
  tokens h = [Bytes (Hash.toByteString h)]

-- | A class for all types which can accumulate tokens into a hash.
-- If you want to provide an instance for hashing a Unison value, see 'Tokenizable'
-- and 'Hashable' instead.
instance Accumulate Hash where
  accumulate = fromBytes . BA.convert . CH.hashFinalize . go CH.hashInit
    where
      go :: CH.Context CH.SHA3_512 -> [Token Hash] -> CH.Context CH.SHA3_512
      go acc tokens = CH.hashUpdates acc (hashingVersion : tokens >>= toBS)
      toBS (Tag b) = [B.singleton b]
      toBS (Bytes bs) = [encodeLength $ B.length bs, bs]
      toBS (Int i) = [BL.toStrict . toLazyByteString . int64BE $ i]
      toBS (Nat i) = [BL.toStrict . toLazyByteString . word64BE $ i]
      toBS (Double d) = [BL.toStrict . toLazyByteString . doubleBE $ d]
      toBS (Text txt) =
        let tbytes = encodeUtf8 txt
         in [encodeLength (B.length tbytes), tbytes]
      toBS (Hashed h) = [Hash.toByteString h]
      encodeLength :: Integral n => n -> B.ByteString
      encodeLength = BL.toStrict . toLazyByteString . word64BE . fromIntegral
  fromBytes = Hash.fromByteString
  toBytes = Hash.toByteString

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
