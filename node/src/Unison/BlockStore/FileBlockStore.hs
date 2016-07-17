{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Unison.BlockStore.FileBlockStore where

import Control.Monad.State
import Control.Monad.Reader
import Data.Acid
import Data.ByteString (ByteString)
import Data.SafeCopy
import Data.Typeable
import Unison.BlockStore.MemBlockStore (makeHash)
import Unison.Hash (Hash)
import Unison.Hash.Extra ()
import qualified Data.Map.Lazy as Map
import qualified Unison.BlockStore as BS

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

deleteSeriesMap :: BS.Series -> Update StoreData ()
deleteSeriesMap series = do
  StoreData hashMap seriesMap <- get
  put (StoreData hashMap (Map.delete series seriesMap))

appendSeriesMap :: BS.Series -> Hash -> Update StoreData ()
appendSeriesMap series hash = do
  StoreData hashMap seriesMap <- get
  put (StoreData hashMap (Map.update (Just . (hash :)) series seriesMap))

readHashMap :: Query StoreData (Map.Map Hash ByteString)
readHashMap = ask >>= (pure . hashMap)

readSeriesMap :: Query StoreData (Map.Map BS.Series [Hash])
readSeriesMap = ask >>= (pure . seriesMap)

$(makeAcidic ''StoreData ['insertHashMap, 'insertSeriesMap, 'deleteSeriesMap, 'appendSeriesMap, 'readHashMap, 'readSeriesMap])

initState :: FilePath -> IO (AcidState StoreData)
initState f = openLocalStateFrom f $ StoreData Map.empty Map.empty

-- think more about threading random state, since it's used two places now
-- TODO implement real garbage collection for hashes removed from series
make :: IO Hash -> AcidState StoreData -> BS.BlockStore Hash
make genHash storeState =
  let insertStore v =
        let hash = makeHash v
        in update storeState (InsertHashMap hash v) >> pure hash
      insert = insertStore
      lookup h = Map.lookup h <$> query storeState ReadHashMap
      declareSeries series = do
        seriesHashes <- Map.lookup series <$> query storeState ReadSeriesMap
        case seriesHashes of
          Nothing -> do
            hash <- genHash
            update storeState $ InsertSeriesMap series [hash]
            pure hash
          Just (h:_) -> pure h
          _ -> error "FileBlockStore.declareSeries had empty list of hashes in series"
      deleteSeries series =
        update storeState $ DeleteSeriesMap series
      update' series hash v = do
        seriesHashes <- Map.lookup series <$> query storeState ReadSeriesMap
        case seriesHashes of
          Just (h:_) | h == hash -> do
                         newHash <- insertStore v
                         update storeState $ InsertSeriesMap series [newHash]
                         pure $ Just newHash
          Just [] -> error "FileBlockStore.update had empty list of hashes in series"
          _ -> pure Nothing
      append series hash v = do
        seriesHashes <- Map.lookup series <$> query storeState ReadSeriesMap
        case seriesHashes of
          Just (h:_) | h == hash -> do
                         newHash <- insertStore v
                         update storeState $ AppendSeriesMap series newHash
                         pure $ Just newHash
          Just [] -> error "FileBlockStore.append had empty list of hashes in series"
          _ -> pure Nothing
      resolve s = (fmap head . Map.lookup s) <$> query storeState ReadSeriesMap
      resolves s = Map.findWithDefault [] s <$> query storeState ReadSeriesMap
  in BS.BlockStore insert lookup declareSeries deleteSeries update' append resolve resolves

make' :: IO Hash -> FilePath -> IO (BS.BlockStore Hash)
make' gen path = initState path >>= pure . make gen
