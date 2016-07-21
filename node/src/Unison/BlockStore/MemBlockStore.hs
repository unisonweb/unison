module Unison.BlockStore.MemBlockStore where


import Data.ByteString (ByteString)
import System.Random
import Unison.Runtime.Address
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

randomAddress :: RandomGen r => r -> (Address, r)
randomAddress = random

makeRandomAddress :: RandomGen r => IORef.IORef r -> IO Address
makeRandomAddress genVar = IORef.atomicModifyIORef genVar ((\(a,b) -> (b,a)) . randomAddress)

make :: (Ord a, Addressor a) =>
  IO a -> IORef.IORef (StoreData a) -> BS.BlockStore a
make genAddress mapVar =
  let insertStore (StoreData hashMap seriesMap rs uc) v =
        let address = makeAddress v
        in (StoreData (Map.insert address v hashMap) seriesMap rs uc, address)
      insert v = IORef.atomicModifyIORef mapVar $ \store ->
        let (ns, newHash) = insertStore store v
        in (ns { permanent = Set.insert newHash (permanent ns)}, newHash)
      lookup h = IORef.readIORef mapVar >>=
        (\(StoreData hashMap _ _ _) -> pure $ Map.lookup h hashMap)
      declareSeries series = do
        hash <- genAddress
        IORef.atomicModifyIORef mapVar $ \store ->
          case Map.lookup series (seriesMap store) of
            Nothing ->
             (store { seriesMap = Map.insert series (SeriesData hash []) (seriesMap store)}
             , hash)
            Just (SeriesData h _) -> (store, h)
      deleteSeries series = IORef.atomicModifyIORef mapVar $ \store ->
        (store { seriesMap = Map.delete series (seriesMap store) }, ())
      update series hash v = IORef.atomicModifyIORef mapVar $ \(StoreData hm sm rc uc) ->
        case Map.lookup series sm of
          Just (SeriesData h _) | h == hash ->
                   let (valueStore, hash) = insertStore (StoreData hm sm rc uc) v
                       newSeries = SeriesData hash [hash]
                       newMap = Map.insert series newSeries $ seriesMap valueStore
                       -- hashes we don't want to garbage collect
                       preserveHashes = Set.union rc . Set.fromList . concatMap seriesList
                         . Map.elems $ newMap
                       clearedMap = Map.filterWithKey (\k _ -> Set.member k preserveHashes)
                         $ hashMap valueStore
                       finalStore = case uc of
                         uc | uc == garbageLimit ->
                           valueStore { seriesMap = newMap
                                      , updateCount = 0
                                      , hashMap = clearedMap }
                         _ -> valueStore { seriesMap = newMap, updateCount = uc + 1 }
                   in (finalStore, Just hash)
          _ -> (StoreData hm sm rc uc, Nothing)
      append series hash v = IORef.atomicModifyIORef mapVar $ \(StoreData hashMap sm rc uc) ->
        case Map.lookup series sm of
          Just (SeriesData h _) | h == hash ->
                   let (valueStore, hash) = insertStore (StoreData hashMap sm rc uc) v
                       newMap = Map.update (Just . addSeriesAddress hash) series
                         $ seriesMap valueStore
                       finalStore = valueStore { seriesMap = newMap }
                   in (finalStore, Just hash)
          _ -> (StoreData hashMap sm rc uc, Nothing)
      resolve s = IORef.readIORef mapVar >>=
        (\(StoreData _ seriesMap _ _) -> pure . fmap lastAddress $ Map.lookup s seriesMap)
      resolves s = IORef.readIORef mapVar >>=
        (\(StoreData _ seriesMap _ _) -> pure . seriesList
          $ Map.findWithDefault (SeriesData undefined []) s seriesMap)
  in BS.BlockStore insert lookup declareSeries deleteSeries update append resolve resolves

make' :: (Ord a, Addressor a) =>  IO a -> IO (BS.BlockStore a)
make' genAddress = IORef.newIORef (StoreData Map.empty Map.empty Set.empty 0)
                >>= pure . make genAddress
