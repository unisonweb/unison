{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Unison.Runtime.ANF.Serialize (serializeGroupV3, serializeGroupV4, serializeValueV3, serializeValueV4, serializeValueLazyV3, serializeValueLazyV4, deserializeGroup, deserializeValue) where

import Control.Monad
import Control.Monad.State.Strict as State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Bytes.Get hiding (getBytes)
import Data.Bytes.Put
import Data.Serialize.Put (runPutLazy)
import Data.Text (Text)
import Unison.Runtime.ANF as ANF hiding (Tag)
import qualified Unison.Runtime.ANF.SerializeV3 as V3
import qualified Unison.Runtime.ANF.SerializeV4 as V4
import qualified Unison.Runtime.SerializeV4 as V4
import qualified Unison.Util.EnumContainers as EC
import Unison.Var (Var)
import Prelude hiding (getChar, putChar)

deserializeGroup :: (Var v) => ByteString -> Either String (SuperGroup v)
deserializeGroup bs = runGetS getVersion bs
  where
    getVersion =
      getWord32be >>= \case
        i
          | i == V3.codeVersion -> V3.getGroup
          | i == V4.codeVersion ->
              flip State.evalStateT V4.emptyDecodeState $ V4.getGroup
        n -> fail $ "deserializeGroup: unknown version: " ++ show n

serializeGroupV3 ::
  (Var v) => EC.EnumMap FOp Text -> SuperGroup v -> ByteString
serializeGroupV3 fops sg = runPutS (putVersion *> V3.putGroup fops sg)
  where
    putVersion = putWord32be V3.codeVersion

serializeGroupV4 ::
  (Var v) => EC.EnumMap FOp Text -> SuperGroup v -> ByteString
serializeGroupV4 fops sg = runPutS (putVersion *> run (V4.putGroup fops sg))
  where
    run = flip State.evalStateT mempty
    putVersion = putWord32be V4.codeVersion

deserializeValue :: ByteString -> Either String Value
deserializeValue bs = runGetS go bs
  where
    go :: (MonadGet m) => m Value
    go =
      getWord32be >>= \case
        3 -> V3.getValue V3.valueVersion
        4 -> flip State.evalStateT V4.emptyDecodeState $ V4.getValue V4.valueVersion
        n
          | n < 1 -> fail $ "deserializeValue: unknown version: " ++ show n
          | n < 3 -> fail $ "deserializeValue: unsupported version: " ++ show n
          | otherwise -> fail $ "deserializeValue: unknown version: " ++ show n

serializeValueV3 :: Value -> ByteString
serializeValueV3 v = runPutS (putVersion *> V3.putValue v)
  where
    putVersion = putWord32be V3.valueVersion

serializeValueV4 :: Value -> ByteString
serializeValueV4 v = runPutS (putVersion *> run (V4.putValue v))
  where
    run = flip State.evalStateT mempty
    putVersion = putWord32be V4.valueVersion

serializeValueLazyV3 :: Value -> L.ByteString
serializeValueLazyV3 v = runPutLazy (putVersion *> V3.putValue v)
  where
    putVersion = putWord32be V3.valueVersion

serializeValueLazyV4 :: Value -> L.ByteString
serializeValueLazyV4 v = runPutLazy (putVersion *> run (V4.putValue v))
  where
    run = flip State.evalStateT mempty
    putVersion = putWord32be V4.valueVersion
