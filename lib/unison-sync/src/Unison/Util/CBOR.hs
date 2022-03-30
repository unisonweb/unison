{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Unison.Util.CBOR where

import Codec.Serialise
import Codec.Serialise.Decoding
import Codec.Serialise.Encoding

data A = A
  { one :: String,
    two :: Int
  }
  deriving (Show)

instance Serialise A where
  encode (A one two) =
    encode one <> encode two
  decode = do
    A <$> decode <*> decode

aBS :: _
aBS = serialise (A "one" 23)

a2BS :: _
a2BS = serialise (A2 "new" 3 True)

data A2 = A2
  { one :: String,
    two :: Int,
    three :: Bool
  }
  deriving (Show)

instance Serialise A2 where
  encode (A2 one two three) =
    encode one <> encode two <> encode three
  decode = do
    A2 <$> decode <*> decode <*> decode

-- A2 <$> decode <*> decode <*> (fromMaybe True <$> optional decode)

encoder1 :: Encoding
encoder1 =
  encode @String "a" <> encode @Int 1

decoder1 :: Decoder s (String, Int)
decoder1 = (,) <$> decode <*> decode

encoder2 :: Encoding
encoder2 = encoder1 <> encode True

decoder2 :: Decoder s (String, Int, Bool)
decoder2 = do
  (a, b) <- decoder1
  c <- decode
  pure (a, b, c)

-- type ConstructorTag = Word8

-- type FieldTag = Word8

-- -- withSerialisable :: (forall a. Serialise a => a -> r) -> Serialisable -> r
-- -- withSerialisable = _

-- -- |
-- type RecordType = [(FieldTag, Encoding)]

-- -- | Each constructor of a sum-type is treated as a record.
-- type SumType = (ConstructorTag, RecordType)

-- encodeSumType :: SumType -> Encoding
-- encodeSumType (tag, record) = encode tag <> encodeRecordType record

-- encodeRecordType :: RecordType -> Encoding
-- encodeRecordType fields =
--   fields
--     & fmap (\(field, val) -> encode field <> val)
--     & fold

-- data MyRecord = MyRecord
--   { fieldOne :: Int,
--     fieldTwo :: MySum
--   }

-- data MySum
--   = A Int
--   | B String
--   | C MyRecord

-- data CRec = CRec {s :: String, i :: Int}

-- encodeSum :: Serialise a => ConstructorTag -> a -> Encoding
-- encodeSum tag r = encode tag <> encode r

-- decodeSum :: (ConstructorTag -> Decoder s a) -> Decoder s a
-- decodeSum selectRecord = do
--   tag <- decode
--   selectRecord tag

-- -- decodeSum' :: (forall r. Serialise r => ConstructorTag -> (r -> a)) -> Decoder s a
-- -- decodeSum' selectRecord = do
-- --   tag <- decode
-- --   thing (selectRecord tag)

-- -- f <$> decode

-- thing :: forall r s a. Serialise r => (r -> a) -> Decoder s a
-- thing f = f <$> decode

-- encodeRecord :: [(FieldTag, Encoding)] -> Encoding
-- encodeRecord xs =
--   encodeListLen (fromIntegral $ length xs)
--     <> foldMap (\(tag, e) -> encode tag <> e) xs

-- -- decodeRecord :: (FieldTag -> () -> Decoder s a) -> Decoder s a

-- -- instance Serialise MyRecord where
-- --   encode (MyRecord a b) = encodeRecord [(0, encode a), (1, encode b)]
-- --   decode = _

-- -- instance Serialise MySum where
-- --   encode = \case
-- --     A n -> encodeSum 0 n
-- --     B s -> encodeSum 1 s
-- --     C r -> encodeSum 2 r

-- --   decode = do
-- --     decodeSum \case
-- --       0 -> A <$> decode
-- --       1 -> B <$> decode
-- --       2 -> C <$> decode
-- --       _ -> fail ""
