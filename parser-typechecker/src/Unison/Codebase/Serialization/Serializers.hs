{-# LANGUAGE RankNTypes #-}

module Unison.Codebase.Serialization.Serializers
  ( TermSerializer(..)
  , DeserializeError(..)
  , roundTrip
  -- serializers
  , v0Serializer
  , v0SerializerCborg
  , v1Serializer
  , v1SerializerCborg
  -- json
  , roundTripJSON
  , v0TermFromJson
  , v1TermFromJson
  , v0TermToJson
  , v1TermToJson
  -- testing
  , getTermFromFile
  , v0TermFromFile
  , v0TermFromFileToJson
  ) where

import           Codec.Serialise.Decoding              (Decoder)
import           Codec.Serialise.Encoding              (Encoding)
import           Codec.CBOR.JSON                       (decodeValue,
                                                        encodeValue)
import qualified Codec.CBOR.Read                       as CBOR
import qualified Codec.CBOR.Write                      as CBOR
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Data.Aeson                            (Value)
import           Data.Bifunctor                        (first, bimap)
import qualified Data.Bytes.Get                        as Get
import qualified Data.Bytes.Put                        as Put
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as BSL
import qualified Data.Serialize.Get                    as Get
import           Data.Text                             (Text, pack)

import qualified Unison.Codebase.Serialization.V0      as V0
import qualified Unison.Codebase.Serialization.V0Cborg as V0Cborg
import qualified Unison.Codebase.Serialization.V1      as V1
import qualified Unison.Codebase.Serialization.V1Cborg as V1Cborg

import           Unison.Parser                         (Ann (External))
import           Unison.Symbol                         (Symbol (..))
import           Unison.Term                           (AnnotatedTerm)

import Debug.Trace (traceShowId)

type Term = AnnotatedTerm Symbol Ann

newtype DeserializeError = DeserializeError Text
  deriving (Eq, Ord, Show)

data TermSerializer = TermSerializer {
   getTerm :: BS.ByteString -> Either DeserializeError Term
 , putTerm :: Term -> BS.ByteString
 }

roundTrip :: TermSerializer -> Term -> Either DeserializeError Term
roundTrip ts = getTerm ts . putTerm ts

roundTripJSON :: TermSerializer -> Term -> Either DeserializeError Term
roundTripJSON ts t = termToJSON ts t >>= termFromJSON ts

-- TODO: how to actually call this?
-- the terms in ".unison/terms/" are compiled with V1 right?
v0Serializer :: TermSerializer
v0Serializer = TermSerializer (deserializeTerm v0Decoder) (serializeTerm v0Encoder)
  where
  v0Decoder :: Get.Get Term
  v0Decoder = V0.getTerm V0.getSymbol (pure External)

  v0Encoder :: Put.MonadPut m => Term -> m ()
  v0Encoder = V0.putTerm V0.putSymbol (const $ pure ())

v1Serializer :: TermSerializer
v1Serializer = TermSerializer (deserializeTerm v1Decoder) (serializeTerm v1Encoder)
  where
  v1Decoder :: Get.Get Term
  v1Decoder = V1.getTerm V1.getSymbol (pure External)

  v1Encoder :: Put.MonadPut m => Term -> m ()
  v1Encoder = V1.putTerm V1.putSymbol (const $ pure ())

v0SerializerCborg :: TermSerializer
v0SerializerCborg =
  TermSerializer (deserializeTermCborg v0DecoderCborg)
                 (serializeTermCborg v0EncoderCborg)
  where
  v0DecoderCborg :: Decoder s Term
  v0DecoderCborg = V0Cborg.getTerm V0Cborg.getSymbol (pure External)

  v0EncoderCborg :: Term -> Encoding
  v0EncoderCborg = V0Cborg.putTerm V0Cborg.putSymbol (const mempty)


v1SerializerCborg :: TermSerializer
v1SerializerCborg =
  TermSerializer (deserializeTermCborg v1DecoderCborg)
                 (serializeTermCborg v1EncoderCborg)
  where
  v1DecoderCborg :: Decoder s Term
  v1DecoderCborg = V1Cborg.getTerm V1Cborg.getSymbol (pure External)

  v1EncoderCborg :: Term -> Encoding
  v1EncoderCborg = V1Cborg.putTerm V1Cborg.putSymbol (const mempty)

--

getTermFromFile :: MonadIO m => TermSerializer -> FilePath -> m (Either DeserializeError Term)
getTermFromFile ts path = getTerm ts <$> liftIO (BS.readFile path)

--

deserializeTerm :: Get.Get Term -> BS.ByteString -> Either DeserializeError Term
deserializeTerm get = first (DeserializeError . pack) . Get.runGetS get

serializeTerm :: (forall m . Put.MonadPut m => a -> m ()) -> a -> BS.ByteString
serializeTerm putTerm t = Put.runPutS (putTerm t)

--

serializeTermCborg :: (Term -> Encoding) -> Term -> BS.ByteString
serializeTermCborg putTerm t = CBOR.toStrictByteString (putTerm t)

deserializeTermCborg :: (forall s. Decoder s a) -> BS.ByteString -> Either DeserializeError a
deserializeTermCborg decoder =
  bimap (DeserializeError . pack . show) snd . CBOR.deserialiseFromBytes decoder . BSL.fromStrict

---
--- Json stuff
---

v0TermFromJson :: Value -> Either DeserializeError Term
v0TermFromJson = termFromJSON v0SerializerCborg

v0TermFromFile :: FilePath -> IO (Either DeserializeError Term)
v0TermFromFile = getTermFromFile v0Serializer

v0TermFromFileToJson :: FilePath -> IO (Either DeserializeError Value)
v0TermFromFileToJson filePath = do
  e <- v0TermFromFile filePath
  return $ traceShowId e >>= v0TermToJson

v0TermToJson :: Term -> Either DeserializeError Value
v0TermToJson = termToJSON v0SerializerCborg

v1TermFromJson :: Value -> Either DeserializeError Term
v1TermFromJson = termFromJSON v1SerializerCborg

v1TermToJson :: Term -> Either DeserializeError Value
v1TermToJson = termToJSON v1SerializerCborg

termToJSON :: TermSerializer -> Term -> Either DeserializeError Value
termToJSON ts = deserializeTermCborg (decodeValue True) . putTerm ts

termFromJSON :: TermSerializer -> Value -> Either DeserializeError Term
termFromJSON ts = getTerm ts . CBOR.toStrictByteString . encodeValue
