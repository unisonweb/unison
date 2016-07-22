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

data SeriesData a = SeriesData { lastAddress :: a, seriesList :: [a] } deriving (Show)

data StoreData a = StoreData
  { hashMap :: !(Map.Map a ByteString)
  , seriesMap :: !(Map.Map BS.Series (SeriesData a))
  , permanent :: Set.Set a
  , orphanCount :: Int
  , updateCount :: Int
  , path :: FilePath
  } deriving (Typeable)

$(deriveSafeCopy 0 'base ''BS.Series)
instance SafeCopy a => SafeCopy (SeriesData a) where
  putCopy (SeriesData la sl) = contain (safePut la >> safePut sl)
  getCopy = contain $ SeriesData <$> safeGet <*> safeGet
instance (Ord a, SafeCopy a) => SafeCopy (StoreData a) where
  putCopy (StoreData hm sm p oc uc path) = contain
    $ safePut hm >> safePut sm >> safePut p >> safePut oc >> safePut uc >> safePut path
  getCopy = contain $ StoreData
    <$> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet <*> safeGet

insertBS :: Ord a => a -> ByteString -> Update (StoreData a) ()
insertBS k v = do
  sd <- get
  let newHashMap = Map.insert k v $ hashMap sd
      newPermanents = Set.insert k $ permanent sd
  put sd { hashMap = newHashMap, permanent = newPermanents }

insertHashMap :: Ord a => a -> ByteString -> Update (StoreData a) ()
insertHashMap k v = do
  sd <- get
  let newHashMap = Map.insert k v $ hashMap sd
  put sd { hashMap = newHashMap }

insertSeriesMap :: Ord a => BS.Series -> SeriesData a -> Update (StoreData a) ()
insertSeriesMap series seriesData = do
  sd <- get
  let newSeriesMap = Map.insert series seriesData $ seriesMap sd
  put sd { seriesMap = newSeriesMap }

deleteSeriesMap ::Ord a =>  BS.Series -> Update (StoreData a) ()
deleteSeriesMap series = do
  sd <- get
  let newSeriesMap = Map.delete series $ seriesMap sd
  put sd { seriesMap = newSeriesMap }

appendSeriesMap :: Ord a => BS.Series -> a -> Update (StoreData a) ()
appendSeriesMap series address = do
  sd <- get
  let newMap = Map.alter alterF series $ seriesMap sd
      alterF Nothing = Just $ SeriesData address [address]
      alterF (Just (SeriesData _ hl)) = Just . SeriesData address $ address : hl
  put sd { seriesMap = newMap }

readHashMap :: Ord a => Query (StoreData a) (Map.Map a ByteString)
readHashMap = ask >>= (pure . hashMap)

readSeriesMap :: Query (StoreData a) (Map.Map BS.Series (SeriesData a))
readSeriesMap = ask >>= (pure . seriesMap)

readPath :: Query (StoreData a) FilePath
readPath = ask >>= (pure . path)

maybeCollectGarbage :: Ord a => Update (StoreData a) ()
maybeCollectGarbage = do
  sd <- get
  let preserveAddresses = Set.union (permanent sd) . Set.fromList . concatMap seriesList
        . Map.elems . seriesMap $ sd
      clearedMap = Map.filterWithKey (\k _ -> Set.member k preserveAddresses) $ hashMap sd
  if orphanCount sd == garbageLimit
    then put sd { hashMap = clearedMap, orphanCount = 0}
    else put sd { orphanCount = orphanCount sd + 1 }

incrementUpdateCount :: Update (StoreData a) Bool
incrementUpdateCount = do
  sd <- get
  let incCount = updateCount sd + 1
      runSnapshot = incCount == checkpointLimit
      newCount = if runSnapshot then 0 else incCount
  put sd { updateCount = newCount}
  pure runSnapshot

$(makeAcidic ''StoreData ['insertHashMap, 'insertSeriesMap, 'appendSeriesMap, 'readSeriesMap, 'insertBS, 'readHashMap, 'readPath, 'maybeCollectGarbage, 'deleteSeriesMap, 'incrementUpdateCount])

maybeCreateCheckpoint :: (SafeCopy a, Typeable a) => AcidState (StoreData a) -> IO ()
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

initState :: (Ord a, Typeable a, SafeCopy a) => FilePath -> IO (AcidState (StoreData a))
initState f = openLocalStateFrom f $ StoreData Map.empty Map.empty Set.empty 0 0 f

make :: (Ord a, Typeable a, SafeCopy a)
  => IO a -> (ByteString -> a) -> AcidState (StoreData a) -> BS.BlockStore a
make genAddress makeAddress storeState =
  let insertStore v =
        let address = makeAddress v
        in update storeState (InsertHashMap address v) >> pure address
      insert v = let address = makeAddress v in do
        update storeState (InsertBS address v)
        maybeCreateCheckpoint storeState
        pure address
      lookup h = Map.lookup h <$> query storeState ReadHashMap
      declareSeries series = do
        seriesAddresses <- Map.lookup series <$> query storeState ReadSeriesMap
        case seriesAddresses of
          Just (SeriesData a _) -> pure a
          Nothing -> do
            address <- genAddress
            update storeState . InsertSeriesMap series $ SeriesData address []
            maybeCreateCheckpoint storeState
            pure address
      deleteSeries series = do
        update storeState $ DeleteSeriesMap series
        update storeState MaybeCollectGarbage
        maybeCreateCheckpoint storeState
      update' series address v = do
        seriesAddresses <- Map.lookup series <$> query storeState ReadSeriesMap
        case seriesAddresses of
          Just (SeriesData a _) | a == address -> do
                         newAddress <- insertStore v
                         update storeState . InsertSeriesMap series
                           $ SeriesData newAddress [newAddress]
                         update storeState MaybeCollectGarbage
                         maybeCreateCheckpoint storeState
                         pure $ Just newAddress
          _ -> pure Nothing
      append series address v = do
        seriesAddresses <- Map.lookup series <$> query storeState ReadSeriesMap
        case seriesAddresses of
          Just (SeriesData a _) | a == address -> do
                         newAddress <- insertStore v
                         update storeState $ AppendSeriesMap series newAddress
                         maybeCreateCheckpoint storeState
                         pure $ Just newAddress
          _ -> pure Nothing
      resolve s = (fmap (head . seriesList) . Map.lookup s)
        <$> query storeState ReadSeriesMap
      resolves s = (seriesList . Map.findWithDefault (SeriesData undefined []) s )
        <$> query storeState ReadSeriesMap
  in BS.BlockStore insert lookup declareSeries deleteSeries update' append resolve resolves

make' :: (Ord a, Typeable a, SafeCopy a)
  => IO a -> (ByteString -> a) -> FilePath -> IO (BS.BlockStore a)
make' gen makeAddress path = initState path >>= pure . make gen makeAddress
