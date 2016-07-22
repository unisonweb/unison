module Unison.BlockStore.MemBlockStore where


import Data.ByteString (ByteString)
import qualified Data.IORef as IORef
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Unison.BlockStore as BS

-- number of updates before unreferenced addresses are garbage collected
garbageLimit :: Int
garbageLimit = 100

data SeriesData a = SeriesData { lastAddress :: a, seriesList :: [a] } deriving (Show)

data StoreData a = StoreData
  { hashMap :: Map.Map a ByteString
  , seriesMap :: Map.Map BS.Series (SeriesData a)
  , permanent :: Set.Set a
  , updateCount :: Int
  } deriving (Show)

addSeriesAddress :: a -> SeriesData a -> SeriesData a
addSeriesAddress h (SeriesData _ sl) = SeriesData h (h:sl)

make :: Ord a => IO a -> (ByteString -> a) -> IORef.IORef (StoreData a)
  -> BS.BlockStore a
make genAddress hash mapVar =
  let insertStore (StoreData hashMap seriesMap rs uc) v =
        let address = hash v
        in (StoreData (Map.insert address v hashMap) seriesMap rs uc, address)
      insert v = IORef.atomicModifyIORef mapVar $ \store ->
        let (ns, newAddress) = insertStore store v
        in (ns { permanent = Set.insert newAddress (permanent ns)}, newAddress)
      lookup h = IORef.readIORef mapVar >>=
        (\(StoreData hashMap _ _ _) -> pure $ Map.lookup h hashMap)
      declareSeries series = do
        address <- genAddress
        IORef.atomicModifyIORef mapVar $ \store ->
          case Map.lookup series (seriesMap store) of
            Nothing ->
             (store {seriesMap = Map.insert series (SeriesData address []) (seriesMap store)}
             , address)
            Just (SeriesData h _) -> (store, h)
      deleteSeries series = IORef.atomicModifyIORef mapVar $ \store ->
        (store { seriesMap = Map.delete series (seriesMap store) }, ())
      update series address v = IORef.atomicModifyIORef mapVar $ \(StoreData hm sm rc uc) ->
        case Map.lookup series sm of
          Just (SeriesData a _) | a == address ->
                   let (valueStore, address) = insertStore (StoreData hm sm rc uc) v
                       newSeries = SeriesData address [address]
                       newMap = Map.insert series newSeries $ seriesMap valueStore
                       -- hashes we don't want to garbage collect
                       preserveAddresses = Set.union rc . Set.fromList . concatMap seriesList
                         . Map.elems $ newMap
                       clearedMap =
                         Map.filterWithKey (\k _ -> Set.member k preserveAddresses)
                         $ hashMap valueStore
                       finalStore = case uc of
                         uc | uc == garbageLimit ->
                           valueStore { seriesMap = newMap
                                      , updateCount = 0
                                      , hashMap = clearedMap }
                         _ -> valueStore { seriesMap = newMap, updateCount = uc + 1 }
                   in (finalStore, Just address)
          _ -> (StoreData hm sm rc uc, Nothing)
      append series address v = IORef.atomicModifyIORef mapVar $ \(StoreData hashMap sm rc uc) ->
        case Map.lookup series sm of
          Just (SeriesData a _) | a == address ->
                   let (valueStore, address) = insertStore (StoreData hashMap sm rc uc) v
                       newMap = Map.update (Just . addSeriesAddress address) series
                         $ seriesMap valueStore
                       finalStore = valueStore { seriesMap = newMap }
                   in (finalStore, Just address)
          _ -> (StoreData hashMap sm rc uc, Nothing)
      resolve s = IORef.readIORef mapVar >>=
        (\(StoreData _ seriesMap _ _) -> pure . fmap lastAddress $ Map.lookup s seriesMap)
      resolves s = IORef.readIORef mapVar >>=
        (\(StoreData _ seriesMap _ _) -> pure . seriesList
          $ Map.findWithDefault (SeriesData undefined []) s seriesMap)
  in BS.BlockStore insert lookup declareSeries deleteSeries update append resolve resolves

make' :: Ord a =>  IO a -> (ByteString -> a) -> IO (BS.BlockStore a)
make' genAddress hash = IORef.newIORef (StoreData Map.empty Map.empty Set.empty 0)
                >>= pure . make genAddress hash
