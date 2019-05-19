{-# LANGUAGE RankNTypes #-}

module Unison.Codebase.Serialization.Testing (test) where

import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
import qualified Codec.CBOR.Read                       as CBOR
import qualified Codec.CBOR.Write                      as CBOR
import           Control.Monad                         (forM)
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import qualified Data.Bytes.Get                        as Get
import qualified Data.Bytes.Put                        as Put
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Lazy                  as BSL
import           Data.List                             (isSuffixOf)
import qualified Data.Serialize.Get                    as Get
import           System.Directory                      (doesDirectoryExist,
                                                        getDirectoryContents)
import           System.FilePath                       ((</>))

import qualified Unison.Codebase.Serialization.V0      as V0
import qualified Unison.Codebase.Serialization.V0Cborg as V0Cborg
import qualified Unison.Codebase.Serialization.V1      as V1
import qualified Unison.Codebase.Serialization.V1Cborg as V1Cborg

import           Unison.Parser                         (Ann (External))
import           Unison.Symbol                         (Symbol (..))
import           Unison.Term                           (AnnotatedTerm)

type Term = AnnotatedTerm Symbol Ann

test :: IO [Bool]
test = do
  terms <- compiledTerms v1Serializer
  forM terms $ \term -> do
    let serializedTerm = putTerm v1SerializerCborg term
        term' = getTerm v1SerializerCborg serializedTerm
        serializedTerm' = putTerm v1Serializer term
        term'' = getTerm v1Serializer serializedTerm'
    putStrLn "V1 term:"
    print term
    putStrLn "V1 CBorg term:"
    print term'
    putStrLn "V1 term == V1 CBorg term:"
    return $ term == term' && term' == term''

compiledTermsFiles :: MonadIO m => m [FilePath]
compiledTermsFiles =
  filter ("compiled.ub" `isSuffixOf`) <$> getRecursiveContents ".unison/terms/"

compiledTerms :: MonadIO m => TermSerializer -> m [Term]
compiledTerms ts = traverse (getTermFromFile ts) =<< compiledTermsFiles

---
--- TermSerializers
---

data TermSerializer = TermSerializer {
   getTerm :: BS.ByteString -> Term
 , putTerm :: Term -> BS.ByteString
 }

getTermFromFile :: MonadIO m => TermSerializer -> FilePath -> m Term
getTermFromFile ts path = getTerm ts <$> liftIO (BS.readFile path)

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
  TermSerializer (deserializeTermCborg v0DecoderCborg . BSL.fromStrict)
                 (serializeTermCborg v0EncoderCborg)
  where
  v0DecoderCborg :: Decoder s Term
  v0DecoderCborg = V0Cborg.getTerm V0Cborg.getSymbol (pure External)

  v0EncoderCborg :: Term -> Encoding
  v0EncoderCborg = V0Cborg.putTerm V0Cborg.putSymbol (const mempty)


v1SerializerCborg :: TermSerializer
v1SerializerCborg =
  TermSerializer (deserializeTermCborg v1DecoderCborg . BSL.fromStrict)
                 (serializeTermCborg v1EncoderCborg)
  where
  v1DecoderCborg :: Decoder s Term
  v1DecoderCborg = V1Cborg.getTerm V1Cborg.getSymbol (pure External)

  v1EncoderCborg :: Term -> Encoding
  v1EncoderCborg = V1Cborg.putTerm V1Cborg.putSymbol (const mempty)

--

deserializeTerm :: Get.Get Term -> BS.ByteString -> Term
deserializeTerm get = either error id . Get.runGetS get

serializeTerm :: (forall m . Put.MonadPut m => a -> m ()) -> a -> BS.ByteString
serializeTerm putTerm t = Put.runPutS (putTerm t)

--

serializeTermCborg :: (Term -> Encoding) -> Term -> BS.ByteString
serializeTermCborg putTerm t = CBOR.toStrictByteString (putTerm t)

deserializeTermCborg :: (forall s. Decoder s Term) -> BSL.ByteString -> Term
deserializeTermCborg decoder =
  either (error . show) snd . CBOR.deserialiseFromBytes decoder

---

getRecursiveContents :: MonadIO m => FilePath -> m [FilePath]
getRecursiveContents topPath =
  liftIO $ do
    names <- getDirectoryContents topPath
    let properNames = filter (`notElem` [".", ".."]) names
    paths <-
      forM properNames $ \name -> do
        let path = topPath </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
          then getRecursiveContents path
          else return [path]
    return (concat paths)
