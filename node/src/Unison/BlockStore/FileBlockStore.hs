{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Unison.BlockStore.FileBlockStore where

import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.ByteString (ByteString)
import Data.SafeCopy
import Data.Typeable
import System.FilePath ((</>))
import Unison.BlockStore.MemBlockStore (makeAddress)
import Unison.Runtime.Address
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified System.Directory as Directory
import qualified Unison.BlockStore as BS

-- number of updates before unreferenced hashes are garbage collected
garbageLimit :: Int
garbageLimit = 100

-- number of updates before deltas are merged into a new snapshot
checkpointLimit :: Int
checkpointLimit = 30

data SeriesData = SeriesData { lastAddress :: Address, seriesList :: [Address] } deriving (Show)

data StoreData = StoreData
  { hashMap :: !(Map.Map Address ByteString)
  , seriesMap :: !(Map.Map BS.Series SeriesData)
  , permanent :: Set.Set Address
  , orphanCount :: Int
  , updateCount :: Int
  , path :: FilePath
  } deriving (Typeable)
$(deriveSafeCopy 0 'base ''Address)
$(deriveSafeCopy 0 'base ''BS.Series)
$(deriveSafeCopy 0 'base ''SeriesData)
$(deriveSafeCopy 0 'base ''StoreData)

insertBS :: Address -> ByteString -> Update StoreData ()
insertBS k v = do
  sd <- get
  let newHashMap = Map.insert k v $ hashMap sd
      newPermanents = Set.insert k $ permanent sd
  put sd { hashMap = newHashMap, permanent = newPermanents }

insertHashMap :: Address -> ByteString -> Update StoreData ()
insertHashMap k v = do
  sd <- get
  let newHashMap = Map.insert k v $ hashMap sd
  put sd { hashMap = newHashMap }

insertSeriesMap :: BS.Series -> SeriesData -> Update StoreData ()
insertSeriesMap series seriesData = do
  sd <- get
  let newSeriesMap = Map.insert series seriesData $ seriesMap sd
  put sd { seriesMap = newSeriesMap }

deleteSeriesMap :: BS.Series -> Update StoreData ()
deleteSeriesMap series = do
  sd <- get
  let newSeriesMap = Map.delete series $ seriesMap sd
  put sd { seriesMap = newSeriesMap }

appendSeriesMap :: BS.Series -> Address -> Update StoreData ()
appendSeriesMap series address = do
  sd <- get
  let newMap = Map.alter alterF series $ seriesMap sd
      alterF Nothing = Just $ SeriesData address [address]
      alterF (Just (SeriesData _ hl)) = Just . SeriesData address $ address : hl
  put sd { seriesMap = newMap }

readHashMap :: Query StoreData (Map.Map Address ByteString)
readHashMap = ask >>= (pure . hashMap)

readSeriesMap :: Query StoreData (Map.Map BS.Series SeriesData)
readSeriesMap = ask >>= (pure . seriesMap)

readPath :: Query StoreData FilePath
readPath = ask >>= (pure . path)

maybeCollectGarbage :: Update StoreData ()
maybeCollectGarbage = do
  sd <- get
  let preserveHashes = Set.union (permanent sd) . Set.fromList . concatMap seriesList
        . Map.elems . seriesMap $ sd
      clearedMap = Map.filterWithKey (\k _ -> Set.member k preserveHashes) $ hashMap sd
  if orphanCount sd == garbageLimit
    then put sd { hashMap = clearedMap, orphanCount = 0}
    else put sd { orphanCount = orphanCount sd + 1 }

incrementUpdateCount :: Update StoreData Bool
incrementUpdateCount = do
  sd <- get
  let incCount = updateCount sd + 1
      runSnapshot = incCount == checkpointLimit
      newCount = if runSnapshot then 0 else incCount
  put sd { updateCount = newCount}
  pure runSnapshot

$(makeAcidic ''StoreData ['insertHashMap, 'insertSeriesMap, 'appendSeriesMap, 'readSeriesMap, 'insertBS, 'readHashMap, 'readPath, 'maybeCollectGarbage, 'deleteSeriesMap, 'incrementUpdateCount])

maybeCreateCheckpoint :: AcidState StoreData -> IO ()
maybeCreateCheckpoint acidState = do
  runSnapshot <- update acidState IncrementUpdateCount
  if runSnapshot
    then do
    storePath <- query acidState ReadPath
    hasArchive <- Directory.doesDirectoryExist $ storePath </> "Archive"
    if hasArchive then Directory.removeDirectoryRecursive $ storePath </> "Archive"
      else pure ()
    createArchive acidState
    createCheckpoint acidState
    else pure ()

initState :: FilePath -> IO (AcidState StoreData)
initState f = openLocalStateFrom f $ StoreData Map.empty Map.empty Set.empty 0 0 f

make :: IO Address -> AcidState StoreData -> BS.BlockStore Address
make genHash storeState =
  let insertStore v =
        let address = makeAddress v
        in update storeState (InsertHashMap address v) >> pure address
      insert v = let address = makeAddress v in do
        update storeState (InsertBS address v)
        maybeCreateCheckpoint storeState
        pure address
      lookup h = Map.lookup h <$> query storeState ReadHashMap
      declareSeries series = do
        seriesHashes <- Map.lookup series <$> query storeState ReadSeriesMap
        case seriesHashes of
          Just (SeriesData h _) -> pure h
          Nothing -> do
            hash <- genHash
            update storeState . InsertSeriesMap series $ SeriesData hash []
            maybeCreateCheckpoint storeState
            pure hash
      deleteSeries series = do
        update storeState $ DeleteSeriesMap series
        update storeState MaybeCollectGarbage
        maybeCreateCheckpoint storeState
      update' series address v = do
        seriesHashes <- Map.lookup series <$> query storeState ReadSeriesMap
        case seriesHashes of
          Just (SeriesData a _) | a == address -> do
                         newHash <- insertStore v
                         update storeState . InsertSeriesMap series
                           $ SeriesData newHash [newHash]
                         update storeState MaybeCollectGarbage
                         maybeCreateCheckpoint storeState
                         pure $ Just newHash
          _ -> pure Nothing
      append series address v = do
        seriesHashes <- Map.lookup series <$> query storeState ReadSeriesMap
        case seriesHashes of
          Just (SeriesData a _) | a == address -> do
                         newHash <- insertStore v
                         update storeState $ AppendSeriesMap series newHash
                         maybeCreateCheckpoint storeState
                         pure $ Just newHash
          _ -> pure Nothing
      resolve s = (fmap (head . seriesList) . Map.lookup s)
        <$> query storeState ReadSeriesMap
      resolves s = (seriesList . Map.findWithDefault (SeriesData undefined []) s )
        <$> query storeState ReadSeriesMap
  in BS.BlockStore insert lookup declareSeries deleteSeries update' append resolve resolves

make' :: IO Address -> FilePath -> IO (BS.BlockStore Address)
make' gen path = initState path >>= pure . make gen
