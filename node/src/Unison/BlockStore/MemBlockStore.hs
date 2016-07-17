module Unison.BlockStore.MemBlockStore where


import Data.ByteString (ByteString)
import System.Random
import Unison.Hash (Hash)
import Unison.Hash.Extra ()
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LB
import qualified Data.Digest.Murmur64 as Murmur
import qualified Data.IORef as IORef
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Unison.BlockStore as BS
import qualified Unison.Hash as Hash

-- number of updates before unreferenced hashes are garbage collected
garbageLimit :: Int
garbageLimit = 100

data SeriesData = SeriesData { lastHash :: Hash, seriesList :: [Hash] } deriving (Show)

data StoreData = StoreData
  { hashMap :: Map.Map Hash ByteString
  , seriesMap :: Map.Map BS.Series SeriesData
  , permanent :: Set.Set Hash
  , updateCount :: Int
  } deriving (Show)

addSeriesHash :: Hash -> SeriesData -> SeriesData
addSeriesHash h (SeriesData _ sl) = SeriesData h (h:sl)

makeHash :: ByteString -> Hash
makeHash = Hash.fromBytes . LB.toStrict
  . Builder.toLazyByteString . Builder.word64LE . Murmur.asWord64 . Murmur.hash64

randomHash :: RandomGen r => r -> (Hash.Hash, r)
randomHash = random

makeRandomHash :: RandomGen r => IORef.IORef r -> IO Hash
makeRandomHash genVar = IORef.atomicModifyIORef genVar ((\(a,b) -> (b,a)) . randomHash)

make :: IO Hash -> IORef.IORef StoreData -> BS.BlockStore Hash
make genHash mapVar =
  let insertStore (StoreData hashMap seriesMap rs uc) v =
        let hash = makeHash v
        in (StoreData (Map.insert hash v hashMap) seriesMap rs uc, hash)
      insert v = IORef.atomicModifyIORef mapVar $ \store ->
        let (ns, newHash) = insertStore store v
        in (ns { permanent = Set.insert newHash (permanent ns)}, newHash)
      lookup h = IORef.readIORef mapVar >>=
        (\(StoreData hashMap _ _ _) -> pure $ Map.lookup h hashMap)
      declareSeries series = do
        hash <- genHash
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
                       newMap = Map.update (Just . addSeriesHash hash) series
                         $ seriesMap valueStore
                       finalStore = valueStore { seriesMap = newMap }
                   in (finalStore, Just hash)
          _ -> (StoreData hashMap sm rc uc, Nothing)
      resolve s = IORef.readIORef mapVar >>=
        (\(StoreData _ seriesMap _ _) -> pure . fmap lastHash $ Map.lookup s seriesMap)
      resolves s = IORef.readIORef mapVar >>=
        (\(StoreData _ seriesMap _ _) -> pure . seriesList
          $ Map.findWithDefault (SeriesData undefined []) s seriesMap)
  in BS.BlockStore insert lookup declareSeries deleteSeries update append resolve resolves

make' :: IO Hash -> IO (BS.BlockStore Hash)
make' genHash = IORef.newIORef (StoreData Map.empty Map.empty Set.empty 0)
                >>= pure . make genHash
