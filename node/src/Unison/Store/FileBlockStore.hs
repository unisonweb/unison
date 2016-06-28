{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeFamilies #-}
module Unison.Store.FileBlockStore where

import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import Data.Acid.Advanced
import Data.ByteString (ByteString)
import Data.SafeCopy
import Data.Typeable
import Debug.Trace (trace)
import System.Random
import Unison.Hash (Hash)
import Unison.Hashable
import Unison.Hash.Extra ()
import qualified Control.Concurrent.MVar as MVar
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import qualified Data.Digest.Murmur64 as Murmur
import qualified Data.Map.Lazy as Map
import qualified Unison.BlockStore as BS
import qualified Unison.Hash as Hash

data StoreData = StoreData
  { hashMap :: !(Map.Map Hash ByteString)
  , seriesMap :: !(Map.Map BS.Series [Hash])
  } deriving (Typeable)
$(deriveSafeCopy 0 'base ''Hash)
$(deriveSafeCopy 0 'base ''BS.Series)
$(deriveSafeCopy 0 'base ''StoreData)

insertHashMap :: Hash -> ByteString -> Update StoreData ()
insertHashMap k v = do
  StoreData hashMap seriesMap <- get
  put (StoreData (Map.insert k v hashMap) seriesMap)

insertSeriesMap :: BS.Series -> [Hash] -> Update StoreData ()
insertSeriesMap series hashes = do
  StoreData hashMap seriesMap <- get
  put (StoreData hashMap (Map.insert series hashes seriesMap))

appendSeriesMap :: BS.Series -> Hash -> Update StoreData ()
appendSeriesMap series hash = do
  StoreData hashMap seriesMap <- get
  put (StoreData hashMap (Map.update (Just . (hash :)) series seriesMap))

readHashMap :: Query StoreData (Map.Map Hash ByteString)
readHashMap = ask >>= (pure . hashMap)

readSeriesMap :: Query StoreData (Map.Map BS.Series [Hash])
readSeriesMap = ask >>= (pure . seriesMap)

$(makeAcidic ''StoreData ['insertHashMap, 'insertSeriesMap, 'appendSeriesMap, 'readHashMap, 'readSeriesMap])

makeHash :: ByteString -> Hash
makeHash = Hash.fromBytes . LB.toStrict
  . Builder.toLazyByteString . Builder.word64LE . Murmur.asWord64 . Murmur.hash64

randomHash :: RandomGen r => r -> (Hash.Hash, r)
randomHash = random

makeRandomHash :: RandomGen r => MVar.MVar r -> IO Hash
makeRandomHash genVar = MVar.modifyMVar genVar (pure . (\(a,b) -> (b,a)) . randomHash)

initStore :: FilePath -> IO (AcidState StoreData)
initStore f = openLocalStateFrom f $ StoreData Map.empty Map.empty

-- think more about threading random state, since it's used two places now
-- TODO implement real garbage collection for hashes removed from series
makeStore :: RandomGen r => MVar.MVar r -> AcidState StoreData -> BS.BlockStore Hash
makeStore genVar storeState =
  let insertStore v =
        let hash = makeHash v
        in update storeState (InsertHashMap hash v) >> pure hash
      insert = insertStore
      lookup h = Map.lookup h <$> query storeState ReadHashMap
      declareSeries series = do
        seriesHashes <- Map.lookup series <$> query storeState ReadSeriesMap
        case seriesHashes of
          Nothing -> do
            hash <- makeRandomHash genVar
            update storeState $ InsertSeriesMap series [hash]
            pure hash
          Just (h:_) -> pure h
      update' series hash v = do
        seriesHashes <- Map.lookup series <$> query storeState ReadSeriesMap
        case seriesHashes of
          Just (h:_) | h == hash -> do
                         newHash <- insertStore v
                         update storeState $ InsertSeriesMap series [newHash]
                         pure $ Just newHash
          Nothing -> pure Nothing
      append series hash v = do
        seriesHashes <- Map.lookup series <$> query storeState ReadSeriesMap
        case seriesHashes of
          Just (h:_) | h == hash -> do
                         newHash <- insertStore v
                         update storeState $ AppendSeriesMap series newHash
                         pure $ Just newHash
          Nothing -> pure Nothing
      resolve s = (fmap head . Map.lookup s) <$> query storeState ReadSeriesMap
      resolves s = Map.findWithDefault [] s <$> query storeState ReadSeriesMap
  in BS.BlockStore insert lookup declareSeries update' append resolve resolves
